
(ns hoeck.rel.sql.relations
  (:use hoeck.rel.operators
        hoeck.rel.conditions
        hoeck.rel.sql
        hoeck.library
        [hoeck.rel.non-lazy :only [get-aggregate-conditions get-group-conditions]]
        [hoeck.rel :only [fields]]
        clojure.contrib.pprint
        clojure.contrib.except)
  (:import (clojure.lang IPersistentSet IFn ILookup)))


;; deftype & protokoll

(deftype sql-relation [_sql _seqd _setd] [IPersistentSet IFn ILookup]
  ;; ILookup
  (.valAt [k] (.get (force _setd) k))
  (.valAt [k nf] (if (contains? (force _setd)) (.get (force _setd) k) nf))
  ;; IPersistentSet
  (.contains [k] (.contains (force _setd) k))
  (.disjoin [k] (.disjoin (force _setd) k))
  (.get [k] (.get (force _setd) k))
  ;; IPersistentCollection
  (.count [] (count (force _seqd)))
  (.cons [o] (cons (force _setd) o))
  (.empty [] (empty (sql-relation _sql {} {})))
  (.equiv [o] (.equiv (force _setd) o))
  ;; Seqable
  (.seq [] (.seq (force _setd)))
  ;; IFn
  (.invoke [arg] (.invoke (force _setd) arg))
  (.invoke [arg brg] (.invoke (force _setd) arg brg))
  (.applyTo [args] (.applyTo (force _setd) args)))

(defmethod relation ::sql-relation [R]
  R) ;; identity

(defmethod relation String [sql-expr & fields]
  (let [sq (delay (sql-query sql-expr))
	st (delay (set (force sq)))]
    (sql-relation sql-expr sq st
                  {:relation-tag :sql
                   :fields (when-not (empty? fields) (set fields))} {})))

(defmethod relation clojure.lang.Symbol [table-name & fields]
  (let [fields (map #(with-meta % {:table table-name}) fields)]
    (apply relation (str "select " (if (empty? fields) 
                                     "*"
                                     (apply str (interpose "," (map sql-symbol fields))))
                         " from " (sql-symbol table-name))
           fields)))


;; retrievable

(defprotocol Retrievable
  (retrieve [r] "Retrieves a new set from the database"))

(extend sql-relation
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
	  (map #(str "(" (._sql %) ") " (sql-symbol (gensym "table"))))
	  (interpose ", ")
	  (apply str " from "))))

(defmethod project :sql
  [R conditions]
  ;; identity-condition: "%s"
  (let [expr (str "select " (apply str (interpose "," (map project-condition-sql conditions)))
		  (from-relation-expr R))]    
    (relation expr)))

(defmethod select :sql
  [R condition]
  (let [expr (str "select *" (from-relation-expr R)
                  " where " (condition-sql-expr condition))]
    (relation expr)))

(defmethod rename :sql
  [R name-newname-map]
  (let [expr (str "select " 
		  (->> R fields
		       (map #(str % " as " (sql-symbol (name-newname-map % %))))
		       (interpose ",")
		       (apply str))
		  (from-relation-expr R))]
    (relation expr)))

(defmethod xproduct :sql
  [R S]
  (let [expr (str "select *" (from-relation-expr R S))]
    (relation expr)))

(defmethod join :sql
  [R S join-condition]
  (let [expr (str "select *" (from-relation-expr R S) " "
                  (join-condition-sql join-condition))]
    (relation expr)))

(defmethod fjoin :sql
  [R f] ;; use derby java-procedures to implement functional join!!!
  ;; or directly put data into derby:
  (fjoin (set R) f))

(defn sql-set-operation
  [operator R S]
  (let [expr (str "select *" (from-relation-expr R)
		  (str " " operator " ")
		  "select *" (from-relation-expr S))]
    (relation expr)))

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
                  " group by " (->> groups (interpose ", ") print-str))]
    (relation expr)))