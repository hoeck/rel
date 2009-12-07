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
	    [hoeck.rel.fields :as fld]
            [hoeck.rel.testdata :as td]
            [clojure.set :as set])
  ;; require hoeck.rel.sql at the bottom
  (:use hoeck.library
        hoeck.rel.conditions
        hoeck.rel.non-lazy
        clojure.contrib.def
        clojure.contrib.duck-streams
        clojure.contrib.pprint))

(declare fields)


;; global relvar store, allows referring to relations with a keyword

(def *relations* {})

(defn relation-or-lookup
  "if a keyword is given, looks it up in *relations*, otherwise just return its argument."
  [keyword-or-relation]
  (if (keyword? keyword-or-relation) 
    (keyword-or-relation *relations*)
    keyword-or-relation))

(defalias relation op/relation)

(defn fields 
  "Return a set of fields of a given relation. Fields are symbols and may have
  metadata, such as type and origin.
  (Note: relation tuples use keywords - should they use symbols too?)"
  [R]
  (with-meta (or (:fields (meta R))
                 (->> R first keys
                      (map #(-> % name symbol))
                      set))
             {:relation-tag :field}))

(defn relation? [R]
  (:relation-tag (meta R)))

(defn- ensure-relation [R]
  (if (relation? R) R (relation R)))

(defmacro rel-operation
  "Plumbing required around relational operations, like field metadata calculation."
  [op [& rels] & op-args]
  ;; operation-schema, eg: (select R     (condition (= ~id 10)))
  ;;      -> (rel-operation select [R]   (condition (= ~id 10)))
  ;;                       (join   R S   (join-condition = :ref-id :id)
  ;;      -> (rel-operation select [R S] (join-condition = :ref-id :id))
  (let [op-args_ (take (count op-args) (repeatedly gensym))
	rels_ (take (count rels) (repeatedly gensym))]
    `(let [~@(mapcat list op-args_ op-args)
	   ~@(mapcat list rels_ 
		     (map (fn [r] `(-> ~r relation-or-lookup ensure-relation)) rels))]
       ;;(with-meta (~op ~@rels_ ~@op-args_)
       ;;                  {:fields (~op ~@(map list (repeat `fields) rels_)
       ;;				~@op-args_)})
       ;;; sql-relations need field information
       (~op ~@rels_ ~@op-args_))))

;; (rel-operation op/select [#{{:id 1} {:id nil}}] (condition ~id))

;; relational operators
;;   make them work on global relvar or on a given relation (a set with a meta)
;;   add some macro foo to make them look prettier
;;   always make a functional (trailing `*') and a macro version of each op
;;   ins arglists: a uppercase R, S or T always denotes a relation or a relvar


(defn rename*
  "Given a map of {:old :new, ...}, rename the fields of R."
  [R name-newname-map]
  (rel-operation op/rename [R] name-newname-map))

(defmacro rename
  "Given a relation or relvar R and pairs of oldname and newname, rename the fields of R."
  [R & name-newname-pairs]
  `(rename* ~R (hash-map ~@name-newname-pairs)))

(defn as
  "rename all fields of a relation R such that they have a common prefix"
  [R prefix]
  (rename* R (zipmap (fields R) (map #(symbol (str prefix "" (name %))) (fields R)))))


;; select

(defn select*
  [R condition]
  (rel-operation op/select [R] condition))

(defmacro select 
  ;; todo: pattern-like matching, eg: [a ? b] matches (condition (and (= *0 a) (= *2 b)))
  ;;       or {:name 'mathias} matches, computes to ->  #{[1 mathias weiland] [2 mathias probst]} == (condition (= *name 'mathias))
  ;;       maybe with nested, destructuring-like expressions like {:name #{mathias, robert}}
  ;;       or [? #{mathias, robert} dresden]
  ;;       or [(< 10 ?) ? (not= #{dresden, rostock})]
  ;;        known as: query-by-example
  ;;        -> use all of {[()]} 
  ;;       multiarg-select: keyword value -> hashmap access like `gete' where (name :keyword) == field-name
  ;;       => qbe ????
  "Macro around select. A Condition is a form wrapped around the condition macro.
  Ex: (select :person (< 20 ~age)) to select all persons of :age greater than 20."
  [R condition]
  `(select* ~R (condition ~condition)))


;; project

(defn project-condition [R thing]
  (cond (keyword? thing) [(identity-condition thing)]
        (symbol? thing) [(identity-condition (keyword thing))]
        (= * thing) (map #(-> % keyword identity-condition) (fields R))
        :else [thing]))

(defn project*
  "Project R according to conditions. Conditions must have a name or must be 
  identity-conditions.
    :field-name generates an identity-condition on :field-name
    * expands into identity-conditions for all fields of R"
  [R & conditions]
  (rel-operation op/project 
		 [R]
		 (mapcat #(project-condition R %)
                         conditions)))

(defmacro project
  "Convienience macro for the project operation:
     :field-name expands to (identity-condition :field-name)
     [expr :name] expands to (condition expr :name)
     * expands into identity-conditions for all fields of R at runtime."
  [R & exprs]
  `(project* ~R
             ~@(map #(cond (or (keyword? %) (symbol? %)) `(identity-condition '~%)
			   (vector? %) `(condition ~(first %) ~(second %))
			   (= '* %) `*
			   :else `(condition ~%))
                    exprs)))

;; union, difference, intersection

(defmacro #^{:private true} def-set-operator 
  "name must be symbol, op must be a symbol to an operation."
  [name op]
  `(defn ~name
     ([R#] R#) ;; call relational identity?
     ([R# S#] (rel-operation ~op [R# S#]))
     ([R# S# & more#] (apply ~name (~name R# S#) more#))))

(def-set-operator union op/union)
(def-set-operator intersection op/intersection)
(def-set-operator difference op/difference)


;; joins

(defn join [R S join-condition]
  (rel-operation op/join 
		 [R S]
		 join-condition))

(defmacro join= [R S a b]
  `(join ~R ~S (join-condition = ~a ~b)))

(defn outer-join [R S join-condition]
  (rel-operation op/outer-join 
		 [R S]
                 join-condition))

(defmacro join= [R S a b]
  `(outer-join ~R ~S (join-condition = ~a ~b)))

(defn fjoin [R f]
  (rel-operation op/fjoin [R] f))


;; crossproduct

(defn xproduct [R S]
  (rel-operation op/xproduct [R S]))


;; aggregate

(defn aggregate*
  "Use aggregate-conditions to reduce over a range of fields.
  To groups fields, use identity-conditions."
  [R & conditions]
  (rel-operation op/aggregate
		 [R]
		 conditions))

(defmacro aggregate
  "conditions are vectors/lists of either functions(1) or
  :sum, :avg, :count, :min or :max keywords and a field-name.
  Single keywords denote fields to group.
  (1) see hoeck.rel.conditions/aggregate-condition-types for 
  definitions of such functions."
  [R & conditions]
  `(aggregate* ~R ~@(map #(cond (or (keyword? %) (symbol? %)) `(identity-condition ~%)
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
  (let [R (set R) ;; make it a clojure relation
        strlen-condition #(fn ([] {:name %}) ([tuple] (-> tuple % pr-str count)))
        fields (fields R)
        max-len-map (first (apply aggregate*
                                  (->> fields
                                       (map keyword)
                                       (map strlen-condition)
                                       (apply project* R))
                                  ;;(apply project* R (map strlen-condition fields))
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
    "#{}"
    (let [fields (map keyword (fields R))
	  restricted-R (if *print-length* (take *print-length* R) R)
	  ;; be shure to get alls values in the same order
	  get-vals (fn [m] (map #(% m) fields))
	  sizes (get-vals (field-sizes restricted-R))
	  values (get-vals (first restricted-R))
	  fmt-str (format-string fields sizes values)
	  s (cl-format nil fmt-str (map get-vals restricted-R))]
      (str "#{" 
	   (.substring s 2 (- (count s) 1))
	   (if (and *print-length* (< *print-length* (count R)))
	     (str \newline "  ...")
	     "")
	   "}"))))

(defn rpprint [R] (binding [*print-length* 15] (-> R pretty-print-relation println)))

;; saving & loading
(comment (defn save-relation [R f]
           (with-out-writer f
	     (print (relation-or-lookup R)))))


;; sql uses fields and some parts of non_lazy and conditions
(require 'hoeck.rel.sql)
(require 'hoeck.rel.sql.relations)
(require 'hoeck.rel.sql.jdbc)
(require 'hoeck.rel.sql.create)
(require 'hoeck.rel.sql.update)



