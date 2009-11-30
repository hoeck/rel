
(ns hoeck.rel.sql.relations
  (:use hoeck.rel.operators
        hoeck.rel.conditions
        hoeck.rel.sql
        hoeck.library
        [hoeck.rel.non-lazy :only [get-aggregate-conditions get-group-conditions]]
        [hoeck.rel :only [fields]]
        clojure.contrib.pprint
        clojure.contrib.except)
  (:require [hoeck.rel.sql.jdbc :as jdbc])
  (:import (clojure.lang IPersistentSet IFn ILookup)))


;; deftype & protokoll

;; relations without a primary key:
(deftype sql-relation [sql setd] [IPersistentSet IFn ILookup]
  ;; ILookup
  (.valAt [k] (.get (force setd) k))
  (.valAt [k nf] (if (contains? (force setd)) (.get (force setd) k) nf))
  ;; IPersistentSet
  (.contains [k] (.contains (force setd) k))
  (.disjoin [k] (sql-relation sql (.disjoin (force setd) k) (meta this) {}))
  (.get [k] (.get (force setd) k))
  ;; IPersistentCollection
  (.count [] (count (force setd)))
  (.cons [o] (sql-relation sql (cons (force setd) o) (meta this) {}))
  (.empty [] (sql-relation sql {} {}))
  (.equiv [o] (.equiv (force setd) o))
  ;; Seqable
  (.seq [] (.seq (force setd)))
  ;; IFn
  (.invoke [arg] (.invoke (force setd) arg))
  (.invoke [arg brg] (.invoke (force setd) arg brg))
  (.applyTo [args] (.applyTo (force setd) args)))


;; relations with primary key:
(deftype indexed-relation [sql-expr imap ikeys] [IPersistentSet IFn ILookup]
  (.hashCode [] (.hashCode (set (keys (force imap)))))
  (.equals [o] (cond (= ::indexed-relation (type o)) (= (force imap) (force (.imap o)))
                     (set? o) (= (seq o) (keys (force imap)))
                     :else false))
  ;; ILookup
  (.valAt [k] (.valAt this k nil))
  (.valAt [k nf] (if (map? k)
		   (.valAt (force imap) (select-keys k ikeys) nf)
		   (.valAt (force imap) k nf)))
  ;; IPersistentSet
  (.contains [k] (boolean (find (force imap)
                                (if (map? k)
                                  (select-keys k ikeys)
                                  k))))
  (.disjoin [k] (if (map? k)
		  (let [pk (select-keys k ikeys)]
		    (indexed-relation. sql-expr
				       (dissoc (force imap) pk)
				       ikeys
				       ;; record deleted tuples
				       (assoc :delete (meta this) (conj (get (meta this) :delete []) k))
				       {})
		    (indexed-relation. sql-expr
				       (dissoc (force imap) pk)
				       ikeys
				       (meta this)
				       {}))))
  (.get [k] (.valAt this k nil))
  ;; IPersistentCollection
  (.count [] (count (force imap)))
  (.cons [o] (indexed-relation. sql-expr
                                (if (map? o)
				  ;; track the change in the tuples metadata
				  (let [pk (select-keys o ikeys)
					tp (get (force imap) pk)]
				    (assoc (force imap) 
				      pk (with-meta (merge tp o)
						    {:update (conj (get (meta tp) :update []) o)})))
				  (assoc (force imap) {} o))
				ikeys
                                (meta this)
                                {}))
  (.empty [] (indexed-relation. sql-expr {} ikeys))
  (.equiv [o] (.equals this o))
  ;; Seqable
  (.seq [] (vals (force imap)))
  ;; IFn
  (.invoke [arg] (.valAt this arg))
  (.invoke [arg brg] (.valAt this arg brg))
  (.applyTo [args] (let [[k nf & r] args]
                     (if r 
                       (throw-arg "Wrong number of arguments to indexed-relation")
                       (.valAt this k nf)))))

(defmethod relation ::indexed-relation [R] R) ;; identity
(defmethod relation ::sql-relation [R] R) ;; identity

(defn unique-index
  "Returns a map of index-tuple to tuple mappings. Throws an exception when
  ks do not form a unique index on xrel."
  [xrel ks]
  (let [i (reduce #(assoc % (select-keys %2 ks) %2) {} xrel)]
    (when (not= (count i) (count xrel))
      (throwf "%s on relation #{%s ...} is not unique, cannot create unique-index"
	      (print-str ks) (first xrel)))
    i))

(defmethod relation String [sql-expr fields & [pkey]]
  (if (or pkey (not (empty pkey)))
    (let [relation-map (delay (unique-index (sql-query sql-expr) pkey))]
      ;; todo: implement metadata on fields using resultsetmetadata?
      (indexed-relation sql-expr
                        relation-map
                        pkey
                        {:fields fields
                         :relation-tag :sql}
                        {}))
    (sql-relation sql-expr (delay (set (sql-query sql-expr)))
                  {:fields fields
                   :relation-tag :sql}
                  {})))

(defmethod relation clojure.lang.Symbol [table-name & fields]
  ;; create a relation from a tablename and some fields
  ;; add field metadata: :datatype, :primary-key and :table
  (let [pkey-set (jdbc/primary-key-columns table-name)
        fields (map #(with-meta % (merge {:table table-name
					  :primary-key (-> % pkey-set boolean)
					  :datatype nil}
					 ;; fields may overwrite generated metadata
					 (meta %)))
                    fields)
        expr (str "select " (if (empty? fields)
                              "*"
                              (apply str (interpose "," (map sql-symbol fields))))
                  " from " (sql-symbol table-name))]
    (relation expr fields (map keyword pkey-set))))


;; retrievable

(defprotocol Retrievable
  (retrieve [r] "Retrieves a new set from the database"))

(extend ::sql-relation
	Retrievable
        {:retrieve (fn [r] (relation (:_sql r)))})

(defn project-condition-sql
  "generate an sql expression for project column-clauses."
  [c]
  (let [m (condition-meta c)
	n (sql-symbol (name (:name m)))]
    (if (= (:type m) :identity)
      n
      (str (condition-sql-expr c) " as " n))))

;; (project-condition-sql (condition (+ ~a "aaaa" ~b)))
;; (project-condition-sql (identity-condition :a))

(defn join-condition-sql
  "generate a sql where clause from a join-condition."
  [join-c]
  (let [cm (condition-meta join-c)]
    (when-not (= (:type cm) :join)
      (throwf "cannot use non :join condition in sql-join"))
    (when-not (:join-symbol cm)
      (throwf "unknown join condition: %s" (:join-function cm)))
    (format "where %s %s %s"
            (sql-symbol (name (:field-a cm)))
            (:join-symbol cm)
            (sql-symbol (name (:field-b cm))))))

;; (join-condition-sql (join-condition = :a :b))

;; sql-generation:
;; project R NAMES    -> select NAMES from (R) r
;; select R CONDITION -> select fields from (S) s where CONDITION
;; X R S              -> select r.fields,s.fields from (R) r, (S) s
;; join R S r s       -> select r.fields,s.fields from (R) r, (S) s where r.r = s.s
;; union R S          -> select r.fields from (R) r union select s.fields from (S) s
;; difference R S     -> select r.fields from (R) r where not exists (select s.fields from (S) s where not r.fields=s.fields)
;; intersection       -> select r.fields from (R) r where not exists (select s.fields from (S) s where r.fields=s.fields)

(defn from-relation-expr
  "given one or more relations, return a string containing a
  sql from-clause."
  ([& rels]
     (->> rels
	  (map #(str "(" (.sql_expr %) ") " (sql-symbol (gensym "table"))))
	  (interpose ", ")
	  (apply str " from "))))

(defn pkey [fields] (map keyword (filter #(-> % meta :primary-key) fields)))

(defmethod project :sql
  [R conditions]
  ;; identity-condition: "%s"
  (let [expr (str "select "
                  (apply str (interpose "," (map project-condition-sql conditions)))
		  (from-relation-expr R))
        fs (project (fields R) conditions)]
    (relation expr fs (pkey fs))))

(defmethod select :sql
  [R condition]
  (let [expr (str "select *" (from-relation-expr R)
                  " where " (condition-sql-expr condition))
        fs (fields R)]
    (relation expr fs (pkey fs))))

(defmethod rename :sql
  [R name-newname-map]
  (let [expr (str "select " 
		  (->> R fields
		       (map #(str % " as " (sql-symbol (name-newname-map % %))))
		       (interpose ",")
		       (apply str))
		  (from-relation-expr R))
        fs (rename (fields R) name-newname-map)]
    (relation expr fs (pkey fs))))

(defmethod xproduct :sql
  [R S]
  (let [expr (str "select *" (from-relation-expr R S))
        fs (xproduct (fields R) (fields S))]
    (relation expr fs (pkey fs))))

(defmethod join :sql
  [R S join-condition]
  (let [expr (str "select *" (from-relation-expr R S) " "
                  (join-condition-sql join-condition))
        fs (join (fields R) (fields S) join-condition)]
    (relation expr fs (pkey fs))))

(defmethod fjoin :sql
  [R f] ;; use derby java-procedures to implement functional join!!!
  ;; or directly put data into derby:
  (fjoin (set R) f))

(defn sql-set-operation
  [operator R S]
  (let [expr (str "select *" (from-relation-expr R)
		  (str " " operator " ")
		  "select *" (from-relation-expr S))
        fs (union (fields R) (fields S))]
    (relation expr fs (pkey fs))))

(defmethod union :sql [R S] (sql-set-operation "union" R S))
(defmethod difference :sql [R S] (sql-set-operation "except"))
(defmethod intersection :sql [R S] (sql-set-operation "intersect"))

(defmethod aggregate :sql [R conditions]
  (let [aggregates (get-aggregate-conditions conditions)
        groups (get-group-conditions conditions)
        agg-selects (map #(let [m (condition-meta %)]
                            (format "%s(%s)"
                                    (name (:function %))
                                    (name (:name m))))
                         aggregates)
        expr (str "select " (->> groups agg-selects
                                 (interpose ", ")
                                 print-str)
                  (from-relation-expr R)
                  " group by " (->> groups (interpose ", ") print-str))
        fs (aggregate (fields R))]
    (relation expr fs (pkey fs))))


(comment;; updateable relations

  (def rr (relation 'person 'id 'status 'name))

  (hoeck.rel/rpprint rr)
  (hoeck.rel/rpprint (conj (conj rr {:name 'foobar})
                           {:name 'bazbak}))

  (map meta (:fields (meta rr)))
  (fields rr)
  (pkey (fields rr))



  (def rr (relation 'personen 'pers_id 'status_id 'name1 'vorname1))
  (hoeck.rel/rpprint rr)
  (pkey (fields rr))

  (def rr1 (conj rr {:pers_id 419 :vorname1 "balke"}))
  (def rr2 (conj rr1 {:pers_id 419 :name1 "Herr"}))
  (map meta rr2)
  
)
