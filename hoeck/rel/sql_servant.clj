;   Copyright (c) 2008, Erik Soehnel All rights reserved.
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

(ns hoeck.rel.sql-servant
  (:require [hoeck.rel :as rel])
  (:use ;[hoeck.rel :only (fields group-by)]
        hoeck.library
        hoeck.rel.sql-utils
        hoeck.rel.sql))

(import '(java.sql SQLException Types))

;; DDL - sql-primitives

(def *query-fn*)

(defn table-exists [name]
  (try (*query-fn* (str "select * from " (name->sql name)))
       :exists
       (catch SQLException e nil)))

(def default-precisions
  {:string 100
   :dec 6})

(defn make-column-def 
  ([col-name type] (make-column-def col-name type (default-precisions type)))
  ([col-name type precision]
     (when-not (type type-string) (throw (Exception. (format "Unknown type %s" type))))
     (str (name->sql col-name) " " (type type-string) (if precision (str "(" precision ")")))))

(defn- create-table-from-vectors
  [table-name fields types]
  (let [create-stmt (format "create table %s (%s)"
                            (name->sql table-name)
                            (add-commata (map #(make-column-def %1 %2) fields types)))]
    (println :create-stmt create-stmt)
    (*query-fn* :command create-stmt)))

(defn create-table
  "Create a new table on *query-fn* with the given name-type* signature."
  [table-name & signature] 
  (let [[fields types] (partition-nth signature)]
    (create-table-from-vectors table-name fields types)))

(defn probe-table [name]
  (with-open [rs (*query-fn* :rs (str "select * from " (name->sql name)))]
             (probe-resultset-r rs)))

(defn drop-table [name]
  (*query-fn* :command (str "drop table " (name->sql name))))

(defn read-table-definition
  "Read a table definition and return relation like probe-table.
  table-definition: column-name typedef
  typedef: type|[type precision]
  type: sql-utils/types
  A table-definition consists of pairs of: column-name typedef
  where typedef if either a type keyword as in sql-utils/types
  or a [type-keyword precision-number] eg.: [:string 100] 
  (for varchar 100 sql decl.) If a necessary precision-value is ommitted,
  the default one from sql-utils/types is used."
  [& sig]
  (let [default-precisions (rel/group-by (rel/select (rel/project types '(type, default-precision)) *default-precision) 'type)]
    (apply rel/make-relation '[nr #^{:primary-key true} field type prec]
           (mapcat (fn [[name type-or-typedef] nr]
                     (let [[type prec] (if (vector? type-or-typedef)
                                         type-or-typedef
                                         [type-or-typedef, (default-precisions type-or-typedef)])]
                       [nr field type prec]))
                   (partition 2 sig) (iterate inc 0)))))

(defn alter-table-types
  "Alter the arity or types of a table on *query-fn*. For specifying 
  table definitions see read-table-definition."
  [table-name & table-definition]
  (let [table-name (name->sql table-name)
        old-def (probe-table table-name)
        change-def (apply read-table-definition table-definition)
        new-def (rel/union old-def
                           (rel/union change-def
                                      (rel/project (rel/outer-join 
                                                             change-def
                                                             (rel/as old-def 'old)
                                                             'field 'old-field)
                                                   '(old-nr field type prec))))]
    new-def

 (read-table-definition 'name [:int 1000] 'foo :int 'second-id :int)
 'old-field 'field)
    (rel/project (rel/outer-join 
                  change-def
                  (rel/as old-def 'old)
                  'field 'old-field)
                 '(old-nr field type prec))))

(alter-table-types 'people 'name [:int 1000] 'foo :int 'second-id :int)
#{[name      :int 1000   1]
  [second-id :int  nil nil]
  [foo       :int  nil nil]}



(probe-table 'people)
#{[0 id         :int     10]
  [1 name       :string 100]
  [3 address-id :int     10]
  [2 vorname    :string 100]}

(read-table-definition 'name [:int 1000] 'foo :int 'second-id :int)



result:
#{[0 id         :int     10]
  [1 name       :int   1000]
  [2 vorname    :string 100]
  [3 address-id :int     10]
  [4 foo        :int    nil]
  [5 second-id  :int    nil]}



(let [x
 (rel/outer-join 
  (rel/as (probe-table 'people) 'old)
  (read-table-definition 'name [:int 1000] 'foo :int 'second-id :int)
  'old-field 'field)]
  x)

     X   Y         Z   A   B     C   D        E       F
#{[nil        nil     nil  nil   2 second-id :int    nil]
  [nil        nil     nil  nil   1 foo       :int    nil]
  [  0 name       :int    1000   1 name      :string 100]
  [  0 id         :int      10 nil       nil     nil nil]
  [  3 address-id :int      10 nil       nil     nil nil]
  [  2 vorname    :string  100 nil       nil     nil nil]}

(merge-fields relation fields merge-condition





(rel/join 
 (rel/as (probe-table 'people) 'old)
 (read-table-definition 'name [:int 1000] 'foo :int 'second-id :int)
 'old-field 'field)
#{[1 name :string 100 0 name :int 1000]}




(defn conditional-project [R fields condition]


;; what to do? first class handling of columns in sql-tables
; COLUMNS: rename, change types, reorder, add, remove
;; goal: manipulate (populated) tables -> preserve data

;alter-table: change types, add columns
;create-table
;drop-table
;remove-column
;copy-table

;copy-column



(alter-table-types 'people 'name [:int 1000] 'foo :int 'second-id :int)

        
        new-sig (merge (into {} (map vector ofields otypes)) alt-sig)
        fields (reduce (fn [r f] (if-not (pos ofields f) (conj r f) r)) ofields (keys alt-sig))
        types (vec (map new-sig fields))
        temp-table-name (name->sql (gensym table-name))]
    (println :create-temp-table ofields otypes)
    (create-table-from-vectors temp-table-name ofields otypes)
    (*query-fn* :command (str "insert into " temp-table-name " select * from " table-name))
    (drop-table table-name)
    (println :create-NEW-table fields types)
    (create-table-from-vectors table-name fields types)
    (do1 (*query-fn* :command (format "insert into %s (%s) select * from %s"
                                      table-name
                                      (add-commata fields)
                                      temp-table-name))
         (drop-table temp-table-name))))



(comment

(rel/group-by (project (probe-table 'people) '()))
(rel/fields (probe-table 'people))
(rel/group-by (probe-table 'people) 'field)

(rel/fields (probe-table 'people))

(def *query-fn* (:query-fn ^hoeck.rel.test/derby-people))

(*query-fn* "select id,name from people where id = 1 union select 2, 'meier'")
(*query-fn* "select distinct 1 as id,2 as name from")
(*query-fn* "select id, (select name from people where id = p.id) from people p")

(hoeck.rel.core/constraints (probe-table 'people))

((hoeck.rel.core/index (probe-table 'people)) 3 100)
((hoeck.rel.core/index 
  (hoeck.rel.core/union
   (probe-table 'people)
   (rel/make-relation '[nr field type prec] 2 'vorname :string 1000)))
 3 100)
))



(let [x (seq (probe-table 'people))]
  (sort-by first > x))




(some identity (map (fn [rel] (if-let [p (primary-key-vec rel)] (map #(pos (fields rel) %) p))) R S))


(doc some)
-------------------------
clojure.core/some
([pred coll])
  Returns the first logical true value of (pred x) for any x in coll,
  else nil.
nil

(some identity '(nil 1 nil))


(probe-table 'people)

(let [a (probe-table 'people)
      b (rel/make-relation '[a b c d] 3 'address-id :string 100)]
  (rel/union (rel/select a (not= *nr 3)) b))
      

(defn insert [A B]
(rel/fields (probe-table 'people))



(create-relation 'people 'name :varchar 'vorname :varchar 'birthday :time 'ort :varchar 'id :int)
(let [{f :fields} (probe-table 'people)] f)
{:fields [id name vorname address-id], :types [:int :string :string :int]}

(-> (cur-qry :conn "") .getMetaData .getSchemaTerm)
(resultvec (-> (cur-qry :conn "") .getMetaData .getSchemas))
(resultvec (-> (*query-fn* :conn "") .getMetaData .getTypeInfo))
[["BIGINT" -5 19 nil nil nil 1 false 2 false false true "BIGINT" 0 0 nil nil 10]
 ["LONG VARCHAR FOR BIT DATA" -4 32700 "X'" "'" nil 1 false 0 true false false "LONG VARCHAR FOR BIT DATA" nil nil nil nil nil]
 ["VARCHAR () FOR BIT DATA" -3 32672 "X'" "'" "length" 1 false 2 true false false "VARCHAR () FOR BIT DATA" nil nil nil nil nil]
 ["CHAR () FOR BIT DATA" -2 254 "X'" "'" "length" 1 false 2 true false false "CHAR () FOR BIT DATA" nil nil nil nil nil]
 ["LONG VARCHAR" -1 32700 "'" "'" nil 1 true 1 true false false "LONG VARCHAR" nil nil nil nil nil]
 ["CHAR" 1 254 "'" "'" "length" 1 true 3 true false false "CHAR" nil nil nil nil nil]
 ["NUMERIC" 2 31 nil nil "precision,scale" 1 false 2 false true false "NUMERIC" 0 31 nil nil 10]
 ["DECIMAL" 3 31 nil nil "precision,scale" 1 false 2 false true false "DECIMAL" 0 31 nil nil 10]
 ["INTEGER" 4 10 nil nil nil 1 false 2 false false true "INTEGER" 0 0 nil nil 10]
 ["SMALLINT" 5 5 nil nil nil 1 false 2 false false true "SMALLINT" 0 0 nil nil 10]
 ["FLOAT" 6 52 nil nil "precision" 1 false 2 false false false "FLOAT" nil nil nil nil 2]
 ["REAL" 7 23 nil nil nil 1 false 2 false false false "REAL" nil nil nil nil 2]
 ["DOUBLE" 8 52 nil nil nil 1 false 2 false false false "DOUBLE" nil nil nil nil 2]
 ["VARCHAR" 12 32672 "'" "'" "length" 1 true 3 true false false "VARCHAR" nil nil nil nil nil]
 ["DATE" 91 10 "DATE'" "'" nil 1 false 2 true false false "DATE" 0 0 nil nil 10]
 ["TIME" 92 8 "TIME'" "'" nil 1 false 2 true false false "TIME" 0 0 nil nil 10]
 ["TIMESTAMP" 93 26 "TIMESTAMP'" "'" nil 1 false 2 true false false "TIMESTAMP" 0 6 nil nil 10]
 ["BLOB" 2004 2147483647 nil nil "length" 1 false 0 nil false nil "BLOB" nil nil nil nil nil]
 ["CLOB" 2005 2147483647 "'" "'" "length" 1 true 1 nil false nil "CLOB" nil nil nil nil nil]
 ["XML" 2009 nil nil nil nil 1 true 0 false false false "XML" nil nil nil nil nil]]

(probe-table )

)
