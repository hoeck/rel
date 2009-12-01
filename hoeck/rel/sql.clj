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
	clojure.contrib.sql.internal
        clojure.contrib.pprint)
  (:require hoeck.rel.non-lazy
            [hoeck.rel.sql.jdbc :as jdbc])
  (:import (clojure.lang IFn ILookup)
           (java.util Collection Set)
	   (java.sql Connection Types)))


;; sql 

(defn sql-quote 
  "Given an object, use (str object) to obtain its printed representation and
  quote it so that an sql-interpreter reads it correctly."
  [s]
  (str \' (-> s str (.replace "\\" "\\\\") (.replace "'" "\\'") (.replace "\"" (str \\ \"))) \'))

(defn sql-symbol
    "Throws an error if (str s) is not a valid sql-identifier
  (table-name, column-name ..), otherwise returns (str s).
  If s is a keyword, use name instead of str."
    [s] 
    (let [s (if (keyword? s) (name s) (str s))
          [n _] (re-matches #"^(\w|\.)+$" s)];; also check for leading digits?
      (or n (throwf "`%s' is not a valid sql-symbol." s))))

(defn sql-print
  "given a literal number, symbol, date keyword or string, print it so that it
  is readable by an sql-interpreter."
  [s]
  (if (number? s)
    (str s)
    (sql-quote s)))


;; sql connections (java.sql.Connection)

(defn set-connection
  "set *db* :connection to a permanent connection."
  [connection-param-map]
  (alter-var-root #'*db* assoc
		  :connection (get-connection connection-param-map)
		  :level 0
		  :rollback (atom false)))

(defn sql-query
  "Executes an sql-expression and returns a resultset-seq"
  ([expr] (sql-query (:connection *db*) expr))
  ([conn expr]
     (when (nil? conn) (throwf "connection is nil, use set-connection to establish one"))
     (with-open [s (.prepareStatement conn expr)]
       (doall (resultset-seq (.executeQuery s))))))

(defn sql-execute
  "Executes an sql-statement and returns nil."
  ([expr] (sql-execute (:connection *db*) expr))
  ([conn expr]
     (when (nil? conn) (throwf "connection is nil, use set-connection to establish one"))
     (with-open [s (.prepareStatement conn expr)]
       (.execute s)
       nil)))

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


;; sql-expressions from hoeck.rel.conditions

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

;;(defmacro relvar
;;  "create a relation from a table and add metadata at compiletime.
;;  Therefor, a *db* :connection must be available at compiletime."
;;  [table-name & columns]
;;  (let [table-name {:primary-key 'foo}]
;;    
;;    ))

