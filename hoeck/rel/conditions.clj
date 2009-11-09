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


(ns hoeck.rel.conditions
  (:use hoeck.library
        [clojure.contrib.except :only [throw-arg]]))

(defn rename-keys [m kmap]
  (if m (into (empty m) (map (fn [[k v]] [(or (kmap k) k), v]) m))))

;; condition

(defn- field-form? 
  "return wether form is a field expr."
  [form]
  (and (seq? form) (= 'clojure.core/unquote (first form))))

;;; generating sql expressions

(defn clj->sql-multiarg-op
  "Converts many-arg operator forms into infix-forms.
  ex: (and a b c) -> (a and b and c)."
  [form]
  (let [op (first form)
        args (next form)]
    (if (< (count args) 2)
      (throw (Exception. (format "need at least 2 arguments for %s" op)))
      (interpose op args))))

(defn clj->sql-2arg-op
  "Converts 2 arg operators from clojure to sql-where clause expressions.
  ex: (= a 1) -> (a = ), (= a b c) -> ((a = b) and (b = c))."
  [form]
  (let [op (first form)
        args (next form)
        two-arg-form #(list (first %) op (second %))]
    (cond (< (count args) 2)
            (throw (Exception. (format "need at least 2 arguments for %s" op)))
          (= (count args) 2)
            (list (first args) op (second args))
          :else
            (interpose 'and (map #(list % op %2) args (next args))))))

(def #^{:doc "Mapping from function symbols to infix expansion fns."}
     sql-condition-ops
  (let [make-op-map (fn [op-expand-fn ops] (into {} (map #(vector %, op-expand-fn) ops)))]
    (merge (make-op-map clj->sql-2arg-op '(= < > <= >= not=))
           (make-op-map clj->sql-multiarg-op '(and or - + * / str))))) ; use str as an alias for string concatenation *shudder*

(def #^{:doc "Maps clojure-functions their symbols."}
     cljfn->sym 
     {= '=
      < '<
      > '>
      <= '<=
      >= '>=
      not= 'not=})

;; condition-object

(defn quote-condition-expression
  "Quote the body of a condition."
  [expr]
  (let [cut-on-quote #(and (list? %) (= (first %) 'quote))
        quote-sql-ops (fn [e] (walk-expr cut-on-quote ; don't quote already quoted forms
                                         #(and
                                           (list? %)
                                           (let [h (first %)]
                                             (and (symbol? h)
                                                  (contains? sql-condition-ops h))))
                                         #(cons 'list (cons (list 'quote (first %)) (next %)))
                                         e))
        quote-fields (fn [e] (walk-expr cut-on-quote
                                        field-form?
                                        #(list 'clojure.core/unquote (-> % second str keyword))
                                        e))]
    (quote-fields (quote-sql-ops expr))))

;;;  The goal of this macro is to provide a function and a readable (and 
;;;  transformable, to sql-infix) expression, both capturing the current
;;;  environment (lexical scope) while preserving enough abstraction from
;;;  the underlying relation implementation and reducing line noise."
(defmacro condition
  "Expand into a fn form implementing a predicate/projection function.
  Calling this function without an arg will return the underlying
  condition-expression, the involved fields, its name, its type and additional
  properties (function meta-data).
  Calling with a hashmap as the arguments executes the expr with the unquoted 
  symbols bound to the corresponding fields of the hashmap.
  Additional metadata may be given as a hashmap, like :name (for projection 
  conditions).
  When Metadata is a single keyword, its assumed to be: {:name the-keyword}."
  ([expr] `(condition ~expr {}))
  ([expr metadata]
     (let [metadata (if (keyword? metadata) {:name metadata} metadata)
           fields (map #(-> % second str keyword) (collect-exprs field-form? expr))
           tuple-sym (gensym "tuple")
           fn-expr (replace-exprs field-form? #(list (-> % second str keyword) tuple-sym nil) expr)]
       `(fn ;~(symbol (name condition-name))
          ([] (merge {:expr (fn [] ~(quote-condition-expression expr)),;; wrap in a fn, so that they are not 'accidentially' computed when accessing the condition-metatdata
                      :fields '~fields
                      :type :user
                      :name '~(gensym "field")}
                     ~metadata))
          ([~tuple-sym]
             ~fn-expr)))))

(defn field-quote
  "Makes a symbol in the hoeck.rel.field namespace for each symbol given.
  Useful for examining condition-expressions (bind to unquote)."
  [field]
  (symbol "hoeck.rel.field" (name field)))

(defmacro condition-meta
  "Return the conditions metadata. When given a keyword, return
  the corresponding value from the condition metadata."
  ([c] `(binding [unquote field-quote]
          (~c)))
  ([c key] `(binding [unquote field-quote]
              ((~c) ~key))))

(defmacro condition-expr
  "Return the expr of condition c with the unquote-fn bound to
  cojure.core/unquote. The default unquote-fn is field-quote."
  ([c] `(condition-expr ~c field-quote))
  ([c unquote-fn] 
     `(binding [unquote ~unquote-fn]
        ((:expr (~c))))))

;; some special conditions

(defn identity-condition
  "A condition which evaluates to the given field-name."
  [name]
  (fn ([] {:expr (clojure.core/unquote name)
           :fields (list name)
           :name name
           :type :identity})
    ([tuple]
       (name tuple))))

(defn join-condition
  "A special condition with evaluates to (f field-of-relation-A field-of-relation-B),
  where f is a symbol. When f is one of the core =, <, >, >=, <= or not= 
  functions, its transatable to an sql-join statement."
  [f name-a name-b]
  (fn ([] {:field-a name-a
           :field-b name-b
           :join-symbol (cljfn->sym f '???)
           :join-function f
           :type :join})
    ([tuple-a tuple-b]
       (f (get tuple-a name-a)
          (get tuple-b name-b)))))

(def aggregate-condition-types
     {:sum #(apply + %)
      :avg #(/ (reduce + %) (count %))
      :count count
      :min #(apply min %)
      :max #(apply max %)})

(defn aggregate-condition
  "Reduces a seq of tuples into a single value. See `aggregate-condition-types' for some sample
  sql-like aggregate-conditions: :sum, :avg, :count, :min and :max
  aggf is either a keyword denoting a standard function or a function, which is fed with a seq
  of tuple fields of field-name."
  [function-or-keyword field-name]
  (let [agg-f (aggregate-condition-types function-or-keyword)]
    (fn ([] {:name field-name
             :type :aggregate
             :function function-or-keyword})
      ([field-seq]	 
         (agg-f (map field-name field-seq))))))

;;(defn order-condition
;;  "Sql only nows ascending and descending order,
;;  clojure relations support any comparator (pred-fn)."
;;  [predicate field-name]
;;  (fn ([] {:name field-name
;;           :function predicate})))