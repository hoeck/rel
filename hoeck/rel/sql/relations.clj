
(ns hoeck.rel.sql.relations
  (:use hoeck.rel.operators
        hoeck.library
        [hoeck.rel :only [fields pretty-print-relation]]
        clojure.contrib.pprint
        clojure.contrib.except)
  (:require [hoeck.rel.sql.jdbc :as jdbc]
            [hoeck.rel.sql.update :as upd]
            [hoeck.rel.sql :as sql]
            [hoeck.rel.expressions :as e]
            [hoeck.rel :as rel]
	    [clojure.set :as set])
  (:import (clojure.lang IPersistentSet IFn ILookup)))


;; deftype & protokoll

;; sql based relation
(deftype SqlRelation [expr setd]
  :as this
  ILookup
  (valAt [k] (.get (force setd) k))
  (valAt [k nf] (if (contains? (force setd)) (.get (force setd) k) nf))
  IPersistentSet
  (contains [k] (.contains (force setd) k))
  (disjoin [k] (SqlRelation. expr (.disjoin (force setd) k) (meta this) {}))
  (get [k] (.get (force setd) k))
  ;; IPersistentCollection
  (count [] (count (force setd)))
  (cons [o] (SqlRelation. expr (conj (force setd) o) (meta this) {}))
  (empty [] (SqlRelation. expr #{} (meta this) {}))
  (equiv [o] (.equiv (force setd) o))
  ;; Seqable
  (seq [] (.seq (force setd)))
  IFn
  (invoke [arg] (.invoke (force setd) arg))
  (invoke [arg brg] (.invoke (force setd) arg brg))
  (applyTo [args] (.applyTo (force setd) args)))

(defn sql-expr
  "Return the relations underlying SQL select expression."
  [sql-relation]
  (.getDynamicField sql-relation :expr ""))

(extend-type ::SqlRelation
  rel/Persistent
  (retrieve ([R] (relation (sql-expr R) (fields R))))
  (store [R]
    (upd/relation-update R)
    (upd/relation-delete R)
    (upd/relation-insert R)
    ;; clear update/delete/insert metadata?
    R))

(defmethod print-method ::SqlRelation [R w]
  ;; be sure to set *print-length*, that avoids accidentially 
  ;; printing a million tuples
  (binding [*print-length* (or *print-length* 15)]
    (.write w (pretty-print-relation R))))

(defmethod relation ::sql-relation [R] R) ;; identity

(defmethod relation String [sql-expression fields]
  (SqlRelation sql-expression (delay (set (sql/query sql-expression)))
               {:fields fields
                :relation-tag :sql}
               {}))

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
                              (apply str (interpose "," (map sql/sql-symbol fields))))
                  " from " (sql/sql-symbol table-name))]
    (relation expr (set fields))))


(defn project-expr-sql
  "generate an sql expression for project column-clauses."
  [expr]
  (let [n (sql/sql-symbol (name (e/get-name expr)))]
    (if (e/identity? expr)
      n
      (str (:expr expr)  " as " n))))

(defn join-expr-sql
  "generate a sql where clause from a join-condition."
  [join-expr]
  (when-not (e/join? join-expr) (throwf "cannot use non :join condition in sql-join"))
  (let [op ({= '= < '< > '> <= '<= >= '>=} (:op join-expr) (:op join-expr))]
    (when-not op (throwf "unknown join condition: %s" (:op join-expr)))
    (format "%s %s %s"
            (sql/sql-symbol (name (:field-a join-expr)))
            op
            (sql/sql-symbol (name (:field-b join-expr))))))

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
	  (map #(str "(" (sql-expr %) ") " (sql/sql-symbol (gensym "table"))))
	  (interpose ", ")
	  (apply str " from "))))

(defmethod project :sql
  [R exprs]
  (let [expr (str "select "
                  (apply str (interpose "," (map project-expr-sql exprs)))
		  (from-relation-expr R))
        fs (project (fields R) exprs)]
    (relation expr fs)))

(defmethod select :sql
  [R select-expr]
  (let [expr (str "select *" (from-relation-expr R)
                  " where " (e/get-expr select-expr))
        fs (fields R)]
    (relation expr fs)))

(defmethod rename :sql
  [R name-newname-map]
  (let [expr (str "select " 
		  (->> R fields
		       (map #(str % " as " (sql/sql-symbol (name-newname-map % %))))
		       (interpose ",")
		       (apply str))
		  (from-relation-expr R))
        fs (rename (fields R) name-newname-map)]
    (relation expr fs)))

(defmethod xproduct :sql
  [R S]
  (let [expr (str "select *" (from-relation-expr R S))
        fs (xproduct (fields R) (fields S))]
    (relation expr fs)))

(defmethod join :sql
  [R S join-expr]
  (let [expr (str "select *" (from-relation-expr R S) " where "
                  (join-expr-sql join-expr))
        fs (join (fields R) (fields S) join-expr)]
    (relation expr fs)))

(defmethod fjoin :sql
  [R f] ;; use derby java-procedures to implement functional join!!!
  ;; or put data directly into derby:
  (fjoin (set R) f))

(defmethod outer-join :sql
  [R S join-expr]
  (let [expr (cl-format nil "select * from (~a) ~a left outer join (~a) ~a on ~a"
                        (sql-expr R) (sql/sql-symbol (gensym 'table))
                        (sql-expr S) (sql/sql-symbol (gensym 'table))
                        (join-expr-sql join-expr))
        fs (join (fields R) (fields S) join-expr)]
    (relation expr fs)))

(defn sql-set-operation
  [operator R S]
  (let [expr (str "select *" (from-relation-expr R)
		  (str " " operator " ")
		  "select *" (from-relation-expr S))
        fs (union (fields R) (fields S))]
    (relation expr fs)))

(defmethod union :sql [R S] (sql-set-operation "union" R S))
(defmethod difference :sql [R S] (sql-set-operation "except"))
(defmethod intersection :sql [R S] (sql-set-operation "intersect"))

(defmethod aggregate :sql [R exprs]
  (let [aggregates (remove e/identity? exprs)
        groups (->> exprs
                    (filter e/identity?)
                    (map e/get-name))
        agg-selects (map #(format "%s(%s)"
                                  (name (e/get-expr %))
                                  (name (e/get-name %)))
                         aggregates)
        expr (str "select " (->> (concat groups agg-selects)
                                 (interpose ", ")
                                 print-str)
                  (from-relation-expr R)
                  " group by " (->> groups (interpose ", ") print-str))
        fs (aggregate (fields R))]
    (relation expr fs)))

(defmethod order-by :sql [R sort-fields]
  (let [expr (str "select *" (from-relation-expr R)
		  " order by " (apply str (interpose ", "
                                                     (map #(str (name %) 
                                                                (if (vector? %)
                                                                  " asc"
                                                                  " desc"))
                                                          sort-fields))))]
    (relation expr (order-by (fields R) sort-fields))))

