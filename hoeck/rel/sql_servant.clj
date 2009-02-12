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
  (let [default-precisions (rel/group-by (rel/select (rel/project-fn types '(type, default-precision)) *default-precision) 'type)]
    (apply rel/make-relation '[pos #^{:primary-key true} field type prec]
           (mapcat (fn [[field type-or-typedef] pos]
                     (let [[type prec] (if (vector? type-or-typedef)
                                         type-or-typedef
                                         [type-or-typedef, (default-precisions type-or-typedef)])]
                       [pos field type prec]))
                   (partition 2 sig) (iterate inc 0)))))


(defn generate-table-definition
  [table-name & table-definition]
  (let [table-name (name->sql table-name)
        old-def (rel/project (probe-table table-name) *pos *field *type *prec [:old *kind])
        change-def (rel/project (apply read-table-definition table-definition) *pos *field *type *prec [:changed *kind])
        changes (rel/project (rel/join change-def
                                       (rel/as old-def 'old)
                                       'field
                                       'old-field)
                             *old-pos *field *type *prec *kind)
        news (let [c (count old-def)]
               (rel/project (rel/difference change-def changes)
                            (+ *pos c) *field *type *prec [:new *kind]))]
    (rel/order-by (rel/union (rel/union old-def news) changes) 'pos)))


(comment

;; what to do? first class handling of columns in sql-tables
; COLUMNS: rename, change types, reorder, add, remove
;; goal: manipulate (populated) tables -> preserve data

;alter-table: change types, add columns
;create-table
;drop-table
;remove-column
;copy-table

;copy-column

(probe-table 'people)

(generate-table-definition 'people 'name [:int 1000] 'foo :int 'second-id :int 'vorname [:string 55])

(*query-fn* "select id  from people ")

)
