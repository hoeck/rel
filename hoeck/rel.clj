; Copyright (c) 2009, Erik Soehnel All rights reserved.
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

(ns hoeck.rel
  (:require [hoeck.rel.operators :as op]
            [hoeck.rel.testdata :as td])
  (:use hoeck.library
        hoeck.rel.conditions
        hoeck.rel.non-lazy
        clojure.contrib.def
        clojure.contrib.duck-streams
        clojure.contrib.pprint))

;; global relvar store, allows referring to relations with a keyword

(def *relations* {})

(defn relation-or-lookup
  "if a keyword is given, looks it up in *relations*, otherwise just return its argument."
  [keyword-or-relation]
  (if (keyword? keyword-or-relation) 
    (keyword-or-relation *relations*)
    keyword-or-relation))

(defalias relation op/relation)
(defalias fields op/fields)

;; relational operators
;;   make them work on global relvar or on a given relation (a set with a meta)
;;   add some macro foo to make them look prettier
;;   always make a functional (trailing `*') and a macro version of each op
;;   ins arglists: a uppercase R, S or T always denotes a relation or a relvar

(defn rename*
  "Given a map of {:old :new, ...}, rename the fields of R."
  [R name-newname-map]
  (op/rename (relation-or-lookup R) name-newname-map))

(defmacro rename
  "Given a relation or relvar R and pairs of oldname and newname, rename the fields of R."
  [R & name-newname-pairs]
  `(rename* ~R (hash-map ~@name-newname-pairs)))

(defn as
  "rename all fields of a relation R such that they have a common prefix"
  [R prefix]
  (rename* R (zipmap (op/fields R) (map #(keyword (str prefix "-" (name %))) (op/fields R)))))


;; select

(defn select*
  [R condition]
  (op/select (relation-or-lookup R) condition))

(defmacro select 
  ;; todo: pattern-like matching, eg: [a ? b] matches (condition (and (= *0 a) (= *2 b)))
  ;;       or {:name 'mathias} matches, computes to ->  #{[1 mathias weiland] [2 mathias probst]} == (condition (= *name 'mathias))
  ;;       maybe with nested, destructuring-like expressions like {:name #{mathias, robert}}
  ;;       or [? #{mathias, robert} dresden]
  ;;       or [(< 10 ?) ? (not= #{dresden, rostock})]
  ;;        known as: query-by-example
  ;;        -> use all of {[()]} 
  ;;       multiarg-select: keyword value -> hashmap access like `get' where (name :keyword) == field-name
  ;;       => qbe ????
  "Macro around select. A Condition is a form wrapped around the condition macro.
  Ex: (select :person (< 20 ~age)) to select all persons of :age greater than 20."
  [R condition]
  `(select* ~R (condition ~condition)))


;; project

(defn project*
  "Project R according to conditions. Conditions must have a name or must be 
  identity-conditions.
    :field-name generates an identity-condition on :field-name
    * expands into identity-conditions for all fields of R"
  [R & conditions]
  (op/project (relation-or-lookup R) (mapcat #(cond (keyword? %) [(identity-condition %)]
						    (= * %) (map identity-condition (op/fields R))
						    :else [%])
					     conditions)))

(defmacro project
  "Convienience macro for the project operation:
     :field-name expands to (identity-condition :field-name)
     [expr :name] expands to (condition expr :name)
     * expands into identity-conditions for all fields of R at runtime."
  [R & exprs]
  `(project* ~R
             ~@(map #(cond (keyword? %) `(identity-condition ~%)
			   (vector? %) `(condition ~(first %) ~(second %))
			   (= '* %) `*
			   :else `(condition ~%))
                    exprs)))

;; union, difference, intersection

(defmacro def-set-operator [name op]
  `(defn ~name [& rels#] (apply ~op (map relation-or-lookup rels#))))

(def-set-operator union op/union)
(def-set-operator intersection op/intersection)
(def-set-operator difference op/difference)


;; joins

(defn join [R S join-condition]
  (op/join (relation-or-lookup R)
           (relation-or-lookup S)
           join-condition))

(defn outer-join [R S join-condition]
  (op/outer-join (relation-or-lookup R)
                 (relation-or-lookup S)
                 join-condition))

(defn fjoin [R f]
  (op/fjoin (relation-or-lookup R) f))


;; xproduct

(defn xproduct [R S]
  (op/xproduct (relation-or-lookup R) (relation-or-lookup S)))


;; aggregate

(defn aggregate*
  "Use aggregate-conditions to reduce over a range of fields.
  To groups fields, use identity-conditions."
  [R & conditions]
  (op/aggregate (relation-or-lookup R) conditions))

(defmacro aggregate
  "conditions are vectors/lists of either functions(1) or
  :sum, :avg, :count, :min or :max keywords and a field-name.
  Single keywords denote fields to group.
  (1) see hoeck.rel.conditions/aggregate-condition-types for 
  definitions of such functions."
  [R & conditions]
  `(aggregate* ~R ~@(map #(cond (keyword? %) `(identity-condition ~%)
                                (or (list? %) (vector? %))
                                  `(aggregate-condition ~(first %) ~(second %)))
                         conditions)))


;; predicates:
                      
(defn like
  "Return true if the expr matches string symbol or keyword x
  Expr is eiter a string or a symbol. It is matched against x ignoring
  case and using `*' as a wildcard."
  [expr x] ;; sql-like-like, match everything easily, simplified regex
  (let [x (if (or (symbol? x) (keyword? x)) (name x) (str x))]
    (.matches (.toLowerCase x) 
              (str "^" (.replace (.toLowerCase (str expr)) "*" ".*") "$"))))

;; pretty printing

(defn- field-sizes
  "Return a map of all :field-name max-size for pretty-printing."
  [R]
  (let [strlen-condition #(fn ([] {:name %}) ([tuple] (-> tuple % pr-str count)))
        fields (op/fields R)
        max-len-map (first (apply aggregate*
                                  (apply project* R (map strlen-condition fields))
                                  (map #(aggregate-condition :max %) fields)))]
    (into {} (map (fn [[k v]] [k (-> k str count (+ 1 v))]) max-len-map))))

(defn- format-string
  "Generate a format-string for pprinting a relation."
  [names, sizes, vals]
  (apply str
         (concat (list "~:{  {")
                 (interpose " "
                            (map (fn [name size val]
                                   (cond (number? val) ;; right-aligned
					   (str "~" size "<" name "~;~s~>")
					 :else ;; left-aligned
					   (str "~" size "@<" name " ~s~>")))
                                 names sizes vals))
                 (list "}~%~}"))))

(defn pretty-print-relation
  "Print R nicely formatted, readable and and aligned into a string.
  Obey to *print-length*."
  [R]
  (if (empty? R)
    #{}
    (let [fields (op/fields R)
	  R (if *print-length* (take *print-length* R) R)
	  get-vals (fn [m] (map #(% m) fields)) ;; be shure to get alls values in the same order
	  sizes (get-vals (field-sizes R))
	  values (get-vals (first R))
	  fmt-str (format-string fields sizes values)
	  s (cl-format nil fmt-str (map get-vals R))]
      (str "#{" (.substring s 2 (- (count s) 1)) (if *print-length* (str \newline "  ...") "") "}"))))

(defn rpprint [R] (binding [*print-length* 15] (-> R pretty-print-relation println)))

;; saving & loading
(comment (defn save-relation [R f]
           (with-out-writer f
             (binding [*pretty-print-relation-opts* (assoc *pretty-print-relation-opts* :max-lines nil :max-linesize nil)]
               (print (relation-or-lookup R))))))

