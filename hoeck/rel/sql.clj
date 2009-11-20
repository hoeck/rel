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
	;; hoeck.rel
	hoeck.rel.operators
        hoeck.rel.conditions
        clojure.walk
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

(def sql-types {String "VARCHAR 200"
                Long "LONG"
                Integer "INT"
                Double "DOUBLE"})

(defn sql-type [class-or-value]
  (let [t (type class-or-value)
        t (if (= t Class) class-or-value t)]
    (sql-types t)))

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
(set-connection {:classname "org.apache.derby.jdbc.EmbeddedDriver"
		 :subprotocol "derby"
		 :subname "/tmp/test.db"
		 ;; symbols: connection properties
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


;; sql-expressions from conditions

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
		  #(cond (or (and (symbol? %)
                                  (not (#{:field :operator} (:sql (meta %)))))
                             (keyword? %))
                         (sql-quote (name %))
                         (string? %)
                         (sql-quote %)
                         :else %)])))

(defn project-condition-sql
  "generate an sql expression for project column-clauses."
  [c]
  (let [m (condition-meta c)
	n (sql-symbol (name (condition-meta c :name)))]
    (if (= (m :type) :identity)
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
	  (map #(str "(" (:_sql %) ") " (sql-symbol (gensym "table"))))
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
		  (->> R hoeck.rel/fields
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

