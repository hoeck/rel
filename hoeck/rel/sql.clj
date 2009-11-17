;   Copyright (c) 2009, Erik Soehnel All rights reserved.
;
;   Redistribution and use in source and binary forms, with or without
;   modification, are permitted provided that the following conditions
;   are met:
;
;     * Redistributions of source code must retain the above copyright
;       notice, this list of conditions and the following disclaimer.
;
;     * Redistributions in binary form must reproduce the above
;       copyright notice, this list of conditions and the following
;       disclaimer in the documentation and/or other materials
;       provided with the distribution.
;
;   THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;   OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;   WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;   ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;   DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;   DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;   GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;   INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;   WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(ns hoeck.rel.sql
  (:use hoeck.library
	hoeck.rel.operators
	clojure.contrib.except
	clojure.contrib.sql
	clojure.contrib.sql.internal)
  (:import (clojure.lang IPersistentSet IFn ILookup)
           (java.util Collection Set)
	   (java.sql Connection Types)))

;; sql utils

(defn sql-quote [s]
  (str \' (-> s str (.replace "\\" "\\\\") (.replace "'" "\\'") (.replace "\"" (str \\ \"))) \'))

(defn sql-symbol
    "Throws an error if (str s) is not a valid sql-identifier
  (table-name, column-name ..), otherwise returns (str s)."
    [s] 
    (let [n (re-matches #"^\w+$" (str s))] ;; also check for leading digits?
      (or n (throwf "`%s' is not a valid sql-name." (str s)))))

;; sql connection

(defn set-connection
  "set *db* :connection to a permanent connection."
  [connection-param-map]
  (alter-var-root #'*db* assoc :connection (get-connection connection-param-map)))

(defn- sql-query
  ([expr] (sql-query (:connection *db*) expr))
  ([conn expr]
     (with-open [s (.prepareStatement conn expr)]
       (doall (resultset-seq (.executeQuery s))))))

(comment 
(set-connection {:classname "com.sybase.jdbc2.jdbc.SybDriver" 
		 :subprotocol "sybase:Tds" ;; ^= what
		 :subname "localhost:2638/temp" ;; ^= where
		 ;; symbols: connection properties
		 'user "dbo"
		 'password "-"
		 'create false})
)

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

(defmethod relation String [sql-expr]
  (let [sq (delay (sql-query sql-expr))
	st (delay (set (force sq)))]
    (sql-relation sql-expr sq st {:relation-tag :sql} {})))

(defmethod relation clojure.lang.Symbol [table-name & fields]
  (relation (str "select " (if fields (apply str (interpose "," (map sql-symbol fields))) "*")
		 " from " (sql-symbol table-name))))

(defprotocol Retrievable
  (retrieve [r] "Retrieves a new set from the database"))

(extend sql-relation
        {:retrieve (fn [r] (relation (:_sql r)))})

;;(use '[hoeck.rel :only (rpprint)])
;;(rpprint (set (relation 'personen 'pers_id 'name)))
;;(do (println "-----")
;;    (dotimes [n 10] (time (reduce + (map :pers_id (relation 'personen 'pers_id))))))


;; sql-expressions

(defn code-walk
  "Like prewalk, but stop walking on quoted forms."
  [f form]
  (if (and (list? form) (= (first form) 'quote))
    form
    (walk (partial code-walk f) identity (f form))))

(defn condition-sql-expr
  "given a condition, return its expression as an sql-string"
  [c]
  (print-str 
	 (reduce #(code-walk %2 %1)
		 (condition-expr c 
				 ;; flag fields (symbol metadata)
				 #(with-meta (-> % name symbol) {:sql :field}))
		 [;; replace str with +
		  #(if (and (seq? %) (= (first %) 'str)) (list* '+ (next %)) %)
		  ;; flag operators
		  #(if (and (seq? %) (symbol? (first %)))
		     (list* (with-meta (first %) {:sql :operator}) (next %))
		     %)
		  ;; expand operators: (and a b c) -> (a and b and c)
		  #(if (seq? %)
		     (if-let [trans (sql-condition-ops (first %))]
		       (trans (list* (with-meta (first %) {:sql :operator}) (next %)))
		       (list* (with-meta (first %) {:sql :function})))
		     %)
		  ;; sql-quote strings
		  #(do (println "XX" % (meta %))
		       ( cond (or (and (symbol? %)
				       (not (#{:field :operator} (:sql (meta %)))))
				  (keyword? %))
			      (sql-quote (name %))
			      (string? %)
			      (sql-quote %)
			      :else %))])))

(defn project-condition-sql
  "generate an sql expression for project column-clauses."
  [c]
  (let [m (condition-meta c)
	n (sql-symbol (name (condition-meta c :name)))]
    (if (= (m :type) :identity)
      n
      (str (condition-sql c) " as " n))))

;; (project-condition-sql (condition (+ ~a "aaaa" ~b)))
;; (project-condition-sql (identity-condition :a))

;; sql-generation:
;; project R NAMES    -> select NAMES from (R) r
;; select R CONDITION -> select fields from (S) s where CONDITION
;; X R S              -> select r.fields,s.fields from (R) r, (S) s
;; join R S r s       -> select r.fields,s.fields from (R) r, (S) s where r.r = s.s
;; union R S          -> select r.fields from (R) r union select s.fields from (S) s
;; difference R S     -> select r.fields from (R) r where not exists (select s.fields from (S) s where not r.fields=s.fields)
;; intersection       -> select r.fields from (R) r where not exists (select s.fields from (S) s where r.fields=s.fields)

(defmethod project :sql
  [R conditions]
  ;; identity-condition: "%s"
  (let [expr (str "select " (apply str (interpose "," (map project-condition-sql conditions)))
		  "  from (" (:_sql R) ") " (sql-symbol (gensym "table")))]
    (relation expr)))

(defmethod select :sql
  [relation condition]
  (let [sql-select (str "select * from (" (:sql-select ^relation) ") " (gensym) " where " (clojure-expr->sql-expr condition))]
    (relation-from-sql (:query-fn ^relation) sql-select ^relation)))

;(def rrr (select sql-people (condition (= *name 'weilandt))))

(defmethod rename :sql
  [relation name-newname-pairs]
  (let [mapping (apply hash-map name-newname-pairs)
        sql-select (str "select " (add-commata (map #(if-let [new (mapping %)] (str % " as " (name->sql new)) (str (name->sql %))) (fields relation)))
                        " from (" (:sql-select ^relation) ") " (gensym))
        m {:fields (vec (map #(or (mapping %) %) (fields relation)))}]
    (relation-from-sql (:query-fn ^relation) sql-select (merge ^relation m))))
    
;(rename sql-people '(name nachname))

(defmethod xproduct :sql
  [R S]
  (let [sql-select (str "select * from"
                        "(" (:sql-select ^R) ") " (gensym) ", "
                        "(" (:sql-select ^S) ") " (gensym))
        m {:fields (into (fields R) (fields S))}]
    (relation-from-sql (:query-fn ^R) sql-select (merge ^R ^S m))))

(defmethod join :sql
  [R S r s]
  (let [sql-select (str "select * from"
                          "(" (:sql-select ^R) ") " (gensym) ", "
                          "(" (:sql-select ^S) ") " (gensym)
                        "where " (name->sql r) " = " (name->sql s))
        m {:fields (into (fields R) (fields S))}]
    (relation-from-sql (:query-fn ^R) sql-select (merge ^R ^S m))))


(defn sql-set-operation
  [operator R S]
  (let [sql-select (str "select * from (" (:sql-select ^R) ") " (gensym)
                        (str " " operator " ")
                        "select * from (" (:sql-select ^S) ") " (gensym))
        m {:fields (into (fields R) (fields S))}]
    (relation-from-sql (:query-fn ^R) sql-select (merge ^R ^S m))))

(defmethod union :sql
  [R S]
  (sql-set-operation "union" R S))

(defmethod difference :sql
  [R S]
  (sql-set-operation "except"))

(defmethod intersection :sql
  [R S]
  (sql-set-operation "intersect"))



(comment 




)