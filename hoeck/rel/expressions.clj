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

;; another try on conditions/query-expressions

(ns hoeck.rel.expressions
  (:use hoeck.library
        clojure.walk
        clojure.contrib.pprint
        [clojure.contrib.except :only [throw-arg, throwf]]))

(defn clj->function 
  "converts a infix function form into sql-function string."
  [[name & args]]
  (format "%s(%s) " name (apply str (interpose "," args))))

(defn clj->multiarg-op
  "Converts many-arg operator forms into infix-forms.
  ex: (and a b c) -> (a and b and c)."
  [[name & args]]
  (cl-format nil (str "(~{~a~^ " name " ~})") args))

(defn clj->2arg-op
  "Converts 2 arg operators from clojure to sql-where clause expressions.
  ex: (= a 1) -> (a = ), (= a b c) -> ((a = b) and (b = c))."
  [[name arg1 arg2 & more]]
  (if (empty? more) 
    (cl-format nil "(~a ~a ~a)" arg1 name arg2)
    (str (clj->2arg-op [name arg1 arg2])
         (clj->2arg-op (cons name more)))))

(defn clj->sql-not [[name expr]] (cl-format nil "not ~a" expr))
(defn clj->sql-is-null [[name expr]] (cl-format nil "is null ~a" expr))
(defn clj->sql-is-not-null [[name expr]] (cl-format nil "is not null ~a" expr))

(def sql-keywords
     (merge (zipmap '(and or) (repeat clj->multiarg-op))
            (zipmap '(+ - = < > <= >= like) (repeat clj->2arg-op))
            (zipmap '(lower upper) (repeat clj->function))
            {'not clj->sql-not
             'is-null clj->sql-is-null
             'is-not-null clj->sql-is-not-null}))

(defn sql-quote 
  "Given an object, use (str object) to obtain its printed representation and
  quote it so that an sql-interpreter reads it correctly."
  [s]
  (str \' (-> s str (.replace "\\" "\\\\") (.replace "'" "\\'") (.replace "\"" (str \\ \"))) \'))

(defn sql-expr [c]
  (postwalk
   (fn [form]
     (cond (seq? form) (if-let [f (sql-keywords (first form))]
                         (f form)
                         form)
           (string? form) (sql-quote form)
           :else form))
   c))

(defn expr*
  ""
  [expr & [name]]
  {:expr (cond (seq? expr) (sql-expr expr)
               (ifn? expr) expr
               :else (throw-arg "expecting fn or seq."))
   :name name})
  
(defn unquote-code-walk
  "Like postwalk, but stop walking on unquoted forms."
  [f form]
  (if (and (seq? form) (= (first form) `unquote))
    (second form)
    (walk (partial unquote-code-walk f) f form)))

(defn syntax-quote-expr
  [expr]
  (unquote-code-walk
   (fn [expr] (cond (seq? expr) (list* 'list expr)
                    (symbol? expr) (list 'quote expr)
                    :else expr))
   expr))

(defmacro expr
  "Macro for creating sql expressions."
  [expr & [name]]
  (let [name (if (nil? name) (keyword (gensym 'expr)) name)]
    `(expr* ~(syntax-quote-expr expr) ~name)))

(comment
    
  (= ((condition* '(and (= id 10) (like (lower 10) "a%"))))
     ((let [a 10] (condition (and (= id 10) (like (lower ~a) "a%"))))))

  (= ((:expr (expr ~#(:a %))) {:a 9}) 9)
  (= ((:expr (expr* #(:a %))) {:a 8}) 8)
  
  )

(defmacro identity-expr [keyw]
  `{:type :identity
    :name ~keyw})

(defmacro join-expr [op key1 key2]
  `{:type :join
    :op '~op
    :field-a ~key1
    :field-b ~key2})

;; expressions

(defn expr? [e]
  (or (keyword? e)
      (map? e)
      (fn? e)))

(defn identity? [e]
  (or (keyword? e) 
      (and (map? e) (-> e :type (= :identity)))))

(defn join? [e]
  (and (map? e) (-> e :type)))

(defn get-expr [e]
  (or (and (map? e) (e :expr))
      (and (fn? e) e)
      (throwf "expression does not yield a expression")))

(defn get-name [e]
  (cond (keyword? e) e
        (map? e) (e :name)
        :else (throwf "cannot extract name from %s" e)))

