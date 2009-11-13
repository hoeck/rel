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
  (:use ;;hoeck.rel.sql-utils
        hoeck.library))

(defn sql-quote [s]
  (str \' (-> s str (.replace "\\" "\\\\") (.replace "'" "\\'") (.replace "\"" (str \\ \"))) \'))
  
(defn pr-str-sql-value
  [x]
  (cond (or (symbol? x) (keyword? x))
          (str "'" (name->sql x) "'")
        (string? x)
          (str "'" x "'")
        :else
          (pr-str x)))

;; sql generation
(defn sql-from-tablename
  "Create sql to access a table."
  ([table-name]  (str "select * from " (name->sql table-name)))
  ([table-name fields]
     (str "select " (apply print-str (map name->sql fields)) " from " (name->sql table-name))))

;;; sql:      
;index-genaration:
;((index relation) 0 10) -> select * from personen where pers_id = 10
;((index relation) 0) -> select * from personen
; seq: select * from (SQL_SELECT) gensym
; get: select first * from (SQL_SELECT) gensym where field_name = %s
; count: select count(*) from (SQL_SELECT) gensym
(defn relation-from-sql
  "Create a HashSet lazily from a sql table (= an sql select expression).
  query-fn is a function which takes an sql-string and returns an resultset" 
  [query-fn sql-select rel-meta]
  (fproxy-relation (merge rel-meta {:sql-select sql-select
                                    :query-fn query-fn
                                    :index (fn index-fn
                                             ([p] (into {} (map (fn [[v]] [v, (index-fn p v)]) (query-fn (format "select distinct %s from (%s) %s" (name->sql (nth (:fields rel-meta) p)) sql-select (name->sql (gensym)))))))
                                             ([p v] (let [a (name->sql (gensym))]                                                      
                                                      (set (query-fn (format "select * from (%s) %s where %s.%s = %s"
                                                                              sql-select a a (name->sql (nth (:fields rel-meta) p)) (pr-str-sql-value v)))))))

                                    })
    {'seq (fn [_] (seq (query-fn sql-select)))
     'count (fn [_] (get-in (query-fn (format "select count(*) from (%s) %s" sql-select (name->sql (gensym)))) [0 0]))
     'contains (let [fields (:fields rel-meta)
                     t (name->sql (gensym))
                     qs (format "select first from (%s) %s where " sql-select t)
                     q-fn (fn [tuple]
                            (apply str qs (butlast (interleave (map #(str t "." (name->sql %) "=" (pr-str-sql-value %2)) fields tuple) (repeat " and ")))))]
                 (fn [_ k] 
                   (if-let [r (query-fn (q-fn k))] (not (empty r)))))
     'get (fn [this k] (if (.contains this k) k))}))

(defmethod make-relation :sql
  [_ sql-fn tablename & initargs]
  (let [{:keys [fields create]} (merge *make-relation-default-initargs* (apply hash-map initargs))
        sql-select-str (apply sql-from-tablename tablename fields)
        m {:relation-tag :sql
           :fields (or (empty?->nil fields) (:fields (with-open [rs (sql-fn :rs sql-select-str)] (probe-resultset rs))))
           :index nil}
        query-fn sql-fn]
    (relation-from-sql query-fn sql-select-str m)))

;(def sql-people (make-relation :sql (make-query-fn :subname "/home/timmy-turner/clojure/test.db" default-derby-args)
;                               'people))
;sql-people
;^sql-people

;sql-generation:
;project R NAMES    -> select NAMES from (R) r
;select R CONDITION -> select fields from (S) s where CONDITION
;X R S              -> select r.fields,s.fields from (R) r, (S) s
;join R S r s       -> select r.fields,s.fields from (R) r, (S) s where r.r = s.s
;union R S          -> select r.fields from (R) r union select s.fields from (S) s
;difference R S     -> select r.fields from (R) r where not exists (select s.fields from (S) s where not r.fields=s.fields)
;intersection       -> select r.fields from (R) r where not exists (select s.fields from (S) s where r.fields=s.fields)

(defmethod project :sql
  [relation projected-fields]
  (let [new-sql-select (str "select " (add-commata (map #(name->sql %) projected-fields))
                            "  from (" (:sql-select (meta relation)) ") " (name->sql (gensym)))
        m {:fields (vec (filter (set projected-fields) (fields relation)))}]
    (relation-from-sql (:query-fn (meta relation)) new-sql-select (merge (meta relation) m))))

(defn clojure-expr->sql-expr
  [condition-object]
  (let [quoted-expr? #(and (list? %) (= (first %) 'quote))
        no-cut (fn [_] false)
        ;; build various transformations consisting of 
        ;; [cut-predicate, replacement-prepdicate, replacement-function]
        fields->fieldnames [quoted-expr? field-name #(name->sql (field-name %))]

        ;; in clojure (str 'a 'b) in sql: (+ "a" "b")
        clojure-str->sql-plus [quoted-expr?
                               #(and (list? %) (= (first %) 'str))
                               #(cons '+ (rest %))]

        infix->prefix [quoted-expr?
                       #(and (list? %) (contains? sql-condition-ops (first %)))
                       #((sql-condition-ops (first %)) %)]
        str-symbols [quoted-expr?
                     #(and (symbol? %) (not (or (field-name %) (sql-condition-ops %))))
                     name]
        str-keywords [quoted-expr?
                      keyword?
                      str]
        quote-strings [no-cut ;quoted-expr?
                       string?
                       #(str "'" % "'")]]
    (print-str
     ;; apply transformations to the source-form of the condition-object
     (reduce (fn [expr [cut pred f]]
               (walk-expr cut pred f expr))
             (first (condition-object))
             [str-keywords str-symbols quote-strings fields->fieldnames clojure-str->sql-plus infix->prefix]))))

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