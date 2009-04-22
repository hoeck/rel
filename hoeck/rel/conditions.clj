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
  (:use hoeck.library))


;; condition

(defn- field? 
  "return wether form is a field expr."
  [form]
  (and (seq? form) (= 'clojure.core/unquote (first form))))


;;; sql stuff

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
    (merge (make-op-map clj->sql-2arg-op '(= < > not=))
           (make-op-map clj->sql-multiarg-op '(and or - + * / str))))) ; use str as an alias for string concatenation *shudder*


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
                                        field?
                                        #(list 'clojure.core/unquote (-> % second str keyword))
                                        e))]
    (quote-fields (quote-sql-ops expr))))

;;;  The goal of this macro is to provide a function and a readable (and 
;;;  transformable, to sql-infix) expression, both capturing the current
;;;  environment (lexical scope) while preserving enough abstraction from
;;;  the underlying relation implementation and reducing line noise."
(defmacro condition
  "expand into a fn form.
  Calling this function without an arg will return the underlying
  condition-expression, the involved columns and additional properties. (function meta-data).
  Calling with a hashmap as the arguments executes the expr with the unquoted symbols bound to
  the corresponding fields of the hashmap."
  ([expr] `(condition ~expr nil ;;~(gensym "c-")
                      ))
  ([expr condition-name]
     (let [fields (map #(-> % second str keyword) (collect-exprs field? expr))
           tuple-sym (gensym "tuple")
           fn-expr (replace-exprs field? #(list (-> % second str keyword) tuple-sym) expr)]
       `(fn ;~(symbol (name condition-name))
          ([]
             ;; Should throw an error if any operator is not in sql-condition-ops.
             ;; Only if all operators used in expr are in sql-condition-ops the expr gets
             ;; quoted correctly.
             ;; If any operator is not in sql-condition-ops, then only introspection of the
             ;; expr is not working, the condition-function-ctor and the
             ;; condition-function are working anyway.
             ;; this is actually function metadata and will be moved there if that finally gets implemented
             {:expr ~(quote-condition-expression expr),
              :fields '~fields
              :type :user
              :name '~(or condition-name 
                          ;; provide a default field-name: ????
                          (keyword (apply str (interpose '- (map name (cons (first expr) fields))))))})
          ([~tuple-sym]
             ~fn-expr)))))

(defmacro condition-meta [c]
  `(binding [unquote (fn [~'field] (symbol "hoeck.rel.field" (name ~'field)))]
     (~c)))


;; some special conditions

(defn make-identity-condition 
  "A condition which evaluates to the given field-name."
  [field-name]
  (fn ([] {:expr (clojure.core/unquote field-name)
           :fields (list field-name)
           :type :identity
           :name field-name})
    ([fields];; should test that field_name is included in fields,
       ;; otherwise throw an error
       (let [field-pos (pos fields field-name)]
         (fn [tuple] (tuple field-pos))))))










