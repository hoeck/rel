
(ns hoeck.rel.sql.relations
  (:use hoeck.rel.operators
        hoeck.rel.conditions
        hoeck.rel.sql
        hoeck.library
        [hoeck.rel.non-lazy :only [get-aggregate-conditions get-group-conditions]]
        [hoeck.rel :only [fields pretty-print-relation]]
        clojure.contrib.pprint
        clojure.contrib.except)
  (:require [hoeck.rel.sql.jdbc :as jdbc]
	    [clojure.set :as set])
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

;; a seq of unique (unique per relation) keys to identify inserted tuples, 
;; used for autogenerated primary keys
;; take 1000: avoid endless if loop someone accidently tries to print the newkeys field of
;; an indexed relation
(def new-pkeys (map #(symbol (apply str %&)) (repeat "new") (take 10000 (iterate inc 0))))

(deftype indexed-relation
  [sql-expr ;; the select expression (string)
   imap ;; ^= map of tables primary-key to rows (tuples)
   ikeys ;; ^= primary key columns
   newkeys] ;; seq of new keys (symbols) to identify new rows where the pkey ist autogenerated
  [IPersistentSet IFn ILookup]
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
				       newkeys
				       ;; record deleted tuples
				       (assoc :deletes (meta this) (conj (get (meta this) :delete []) k))
				       {}))
		  (throwf "Only tuples may be used as keys to a relation, not %s" k)))
  (.get [k] (.valAt this k nil))
  ;; IPersistentCollection
  (.count [] (count (force imap)))
  (.cons [o] ;; only allow assoc of tuples!
         (when-not (map? o) (throwf "Only tuples may be conjed onto a relation, not %s" o))
         (let [pk (select-keys o ikeys)
               new? (not (find (force imap) pk))
	       tp (if new?
		    (merge (zipmap ikeys newkeys) o) ;; new tuple
		    (get (force imap) pk))]
	   ;;(println :new  new? :tp tp :pk pk)
	   (if new?
	     ;; insert new `row' into relation
	     (indexed-relation. sql-expr
				(assoc (force imap) (select-keys tp ikeys) tp)
				ikeys
				(drop (count ikeys) newkeys) ;; drop generated keys
				(let [m (meta this)] 
				  (assoc m :inserts (conj (:inserts m #{}) (select-keys tp ikeys))))
				{})
	     ;; track the change in the tuples metadata
	     (indexed-relation. sql-expr
				(assoc (force imap) 
				  pk (with-meta (merge tp o)
						{:update (conj (get (meta tp) :update []) o)}))
				ikeys				
				newkeys
				(let [m (meta this)] ;; record updated pkeys in R's metadata
				  (assoc m :updates (conj (:updates m #{}) pk)))
				{}))))
  (.empty [] (indexed-relation. sql-expr {} ikeys newkeys))
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

;; do print indexed-relation as a set, to avoid printing the long new-pkeys seq
(defmethod print-method ::indexed-relation [R w]
  (binding [*print-length* (or *print-length* 15)]
    (.write w (pretty-print-relation R))))

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
			new-pkeys
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
  ;; when fields are nil, use jdbc to determine fields all table-fields
  ;; if one field is * or '*, then merge the given fields with fields from jdbc-metadata
  (let [pkey-set (jdbc/primary-key-columns table-name)
	star? (or (empty? fields) (some #{* '*} fields))
	assigned-fields (set fields)
	jdbc-fields (map #(vary-meta % merge (-> % assigned-fields meta))
			 (jdbc/table-fields table-name))
	fields (if star?
		 jdbc-fields
		 (filter assigned-fields jdbc-fields))
        expr (str "select " (if (empty? fields)
                              "*"
                              (apply str (interpose "," (map sql-symbol fields))))
                  " from " (sql-symbol table-name))]
    (relation expr (set fields) (map keyword pkey-set))))


;; retrievable

(defprotocol Retrievable
  (retrieve [r] "Retrieves a new set from the database"))

(extend ::sql-relation
	Retrievable
        {:retrieve (fn [r] (relation (.sql_expr r)))})

(extend ::indexed-relation
	Retrievable
        {:retrieve (fn [r] (relation (.sql_expr r) (fields r) (.ikeys r)))})

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
    (format "%s %s %s"
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
  (let [expr (str "select *" (from-relation-expr R S) " where "
                  (join-condition-sql join-condition))
        fs (join (fields R) (fields S) join-condition)]
    (relation expr fs (pkey fs))))

(defmethod fjoin :sql
  [R f] ;; use derby java-procedures to implement functional join!!!
  ;; or put data directly into derby:
  (fjoin (set R) f))

(defmethod outer-join :sql
  [R S join-condition]
  (let [expr (cl-format nil "select * from (~a) ~a left outer join (~a) ~a on ~a"
                        (.sql_expr R) (sql-symbol (gensym 'table))
                        (.sql_expr S) (sql-symbol (gensym 'table))
                        (join-condition-sql join-condition))
        fs (join (fields R) (fields S) join-condition)]
    (relation expr fs (pkey fs))))

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



  (def rr (relation 'personen (with-meta 'pers_id {:autoincrement :true}) 'status_id 'name1 'vorname1))
  (hoeck.rel/rpprint rr)
  (map meta (fields rr))

  (def rr1 (conj rr {:pers_id 419 :vorname1 "balke"}))
  (def rr2 (conj rr1 {:pers_id 419 :name1 "Herr"}))
  (map meta rr2)
  
)
