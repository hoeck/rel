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

;;; relational algebra for clojure, third system: sets & hash-indices

(ns hoeck.rel.core
  (:require [clojure.set :as set])
  (:use hoeck.library, clojure.contrib.fcase)
  (:import (clojure.lang Delay)))

;; primitive accessors:

(defn single? [k] (when (not (rest k)) (first k)))

(defn index
  "Return the index of relation"
  ([relation]
   (:index (meta relation))))

(defn fields
  ([relation]
   (:fields (meta relation))))

(defn constraints
  "Return a map of constriants and their values."
  [relation]
  {:primary-key (into [] (filter #(:primary-key (meta %)) (fields relation))})
;;  (reduce #(let [c (select-keys (meta %2) '(:primary-key))] (if (empty? c) % (conj % [%2 c]))) {} (fields relation)))



(defn primary-key-vec
  "Return a vector of the fields in the primary-key or nil if there isn't one."
  [relation]
  (empty?->nil (vec (filter #(:primary-key (meta %)) (fields relation)))))

(defn get-entry-accesssor
  "Return a function that accesses the field NUM of RELATION."
  [relation num]
  (if (single? (first relation)) identity #(nth % num)))

(defn arity
  "Return the arity of a relation by counting either the fields or looking at
  the arity of the firs tuple."
  [relation]
  (or (when-let [f (empty?->nil (fields relation))] (count f))
      (when-let [tup (first relation)] (if (coll? tup) (count tup) 1))))

;; assertions:

(defn check-fields!
  "Throw Exception if any field is not in R."
  [R fields-to-test]
  (if-let [u (remove (set (fields R)) fields-to-test)]
      (throw (Exception. (format "Field %s is not known" (first u))))
    true))

;; tools:

(defn into-tuple
  "Return a vector of element or elements (if v,k are coll?) v,k in order."
  ([v] (if (coll? v) v [v]))
  ([v k]
     (if (coll? v)
       (if (coll? k)
         (reduce conj v k)
         (conj v k))
       (if (coll? k)
         (vec (cons v k))
         [v k])))
  ([v k & r]
     (reduce into-tuple (into-tuple v k) r)))

(defn subnvec
  "Return a vector consisting of elements at elements in v.
ex: (subnvec '[a b c d e] [0 2 3]) -> [a c d]"
  [v elements]
  (vec (map #(nth v %) elements)))

(defn subnvec-or-value
  "Like subnvec but return a single element if (count elements) is 1."
  [v elements]
  (if (vector? v)
    (if (single? elements) (nth v (nth elements 0)) (subnvec v elements))
    v))

(defn dissoc-vec
  "Opposite of assoc for PersistentVector."
  [v i]
  (cond (= 0 i) (subvec v 1)
        (= (dec (count v)) i) (subvec v 0 (dec (count v)))
        :else (vec (concat (subvec v 0 i) (subvec v (inc i))))))

(defn dissocn-vec
  "Dissoc-vec multiple elements at a time."
  [v i]
  (let [i (sort i)] 
    (reduce #(dissoc-vec %1 %2) v (map - i (counter)))))

(defn set-intersection
  "like clojure.set/intersection but may intersect more than 2 sets."
  ([x y] (clojure.set/intersection x y))
  ([x y & z] (reduce clojure.set/intersection (list* x y z))))

(defn multi-index-lookup
  "look up multiple names using their single indexes"
  ([index-fn positions vals]
     (empty?->nil (apply set-intersection (map #(set (index-fn %1 %2)) positions vals)))))

(defn lookup
  "given one or more keys at pos, find index-entries using index-fn."
  [index-fn positions vals]
  (if-let [f (single? positions)] (index-fn f vals) (multi-index-lookup index-fn positions vals)))

(defn apply-key
  "Given key-pos and a seq of vectors, create a hashmap using nth elements
  of each vector as the key and the vector itself minus the keys as the value.
  Use the first tuple to determine the number of elements."
  [tuple-seq keys]
  (let [single-element? (= (count (first tuple-seq)) (+ 1 (count keys)))
        single-element-index (if single-element? (first (set/difference (set (range 0 (count (first tuple-seq)))) (set keys))))]
  (if-let [key (single? keys)]
    (into {} (map #(vector (nth % key)
                           (if single-element?
                               (nth % single-element-index)
                               (dissoc-vec % key)))
                    tuple-seq))
    (into {} (map #(vector (subnvec % keys)
                           (if single-element?
                               (nth % single-element-index)
                               (dissocn-vec % keys)))
                  tuple-seq)))))

(defn make-index-on-hashmap
  "Return a hashmap that represents an index to the keys of src-map on entry-accessor (a function which accesses atimoic from src-map)."
  ([src-map entry-accessor] ;; index on single field
   (reduce #(let [value (entry-accessor %2)
                  index (get % value #{})]
              (assoc % value (conj index (key %2))))
           {}
           src-map))
  ([src-map entry-accessor & more-entry-accessors]  ;; index on multiple fields
   (reduce #(let [value (vec (map (fn [accessor] (accessor %2)) (cons entry-accessor more-entry-accessors)))
                  index (get % value #{})]
              (assoc % value (conj index (key %2))))
           {}
           src-map)))

(defn make-index-on-set
  "Return a hashmap that represents an index on entry-accessor in src-set"
  ([src-set entry-accessor] ;; index on single field
   (reduce #(let [value (entry-accessor %2)
                  index (get % value #{})]
              (assoc % value (conj index %2)))
           {}
           src-set))
  ([src-set entry-accessor & more-entry-accessors]  ;; index on multiple fields
   (reduce #(let [value (vec (map (fn [accessor] (accessor %2)) (cons entry-accessor more-entry-accessors)))
                  index (get % value #{})]
              (assoc % value (conj index %2)))
           {}
           src-set)))

(defn make-index-on-relation
  "Return a delayed hashmap of fields mapping to delayed index-hashmaps generated with make-index-on."
  [relation]
  (if (not (vector? (first relation)))
    (delay [(delay (into {} (map #(vector % %) relation)))])
    (delay (into [] (map #(Delay. (fn [] (make-index-on-set relation (get-entry-accesssor relation %))))
                         (range 0 (count (first relation))))))))
  
(defn make-index-fn
  "Returns a function that wraps the (delayed) index-map according to the index-protocol."
  [delayed-index-vector]
  (fn index ([field] (force ((force delayed-index-vector) field)))
             ([field value] ((index field) value))))

(defn add-index [relation]
  (with-meta relation (assoc (meta relation) :index (make-index-fn (make-index-on-relation relation)))))

; (defmacro fproxy-relation [meta & body]
;   `(fproxy [clojure.lang.APersistentSet] [~meta {}]
;     (let [methods# ~@body]
;       (assoc methods# ~''withMeta
;              (fn with-meta [this# m#]
;                (fproxy [clojure.lang.APersistentSet] [m# {}]
;                         (assoc methods# ~''withMeta ~'with-meta)))))))

(defmacro fproxy-relation [meta methods]
  `(hoeck.rel.Relation. ~meta ~methods))

(def *make-relation-default-initargs* {})

(defmulti make-relation (fn [dispatch-arg & initargs] 
                            (cond (keyword? dispatch-arg)
                                    dispatch-arg
                                  :else
                                    (class dispatch-arg))))

(defmethod make-relation clojure.lang.IPersistentSet
  [data & initargs]
  (let [{:keys [fields]} (merge *make-relation-default-initargs* (apply hash-map initargs))
        relation-meta {:relation-tag :clojure :fields fields} ;; to come: sql, pivot, file, filesys??
        compiled-relation (with-meta data relation-meta)]
    (add-index compiled-relation)))

(defmethod make-relation java.lang.Number
  [arity & data]
  (let [data (partition arity data)
        fields (vec (first data))]
    (make-relation (into #{} (map vec (rest data))) :fields fields)))

(defmethod make-relation clojure.lang.IPersistentVector
  [fields & data]
  (make-relation (set (map vec (partition (count fields) data))) :fields fields))

(defn empty-relation [fields]
  (make-relation #{} :fields (vec fields)))

;; condition
(defn make-coded-symbol-predicate
  "Given a regex and group number, return a function that returns the 
  matched group element group-nr on a given symbol."
  [regex, group-nr]
  (let [pattern (re-pattern regex)]
    (fn [sym] 
      (if-let [[g] (re-seq pattern (str sym))] (symbol (nth g group-nr)) nil))))

(def #^{:doc 
  "Return the fieldname of a symbol or nil of its not a field.
  Fieldnames are symbols starting with `*' and ending with any other character than `*'."}
     field-name (make-coded-symbol-predicate "^(\\*)([^\\*]*)$" 2))

(def sample-expr '(or (= *name name) (= *address-id 100)))

;;; sql stuff
(defn clj->sql-multiarg-op
  "Converts many-arg operator forms into infix-forms.
  ex: (and a b c) -> (a and b and c)."
  [form]
  (let [op (first form)
        args (rest form)]
    (if (< (count args) 2)
      (throw (Exception. (format "need at least 2 arguments for %s" op)))
      (interpose op args))))

(defn clj->sql-2arg-op
  "Converts 2 arg operators from clojure to sql-where clause expressions.
  ex: (= a 1) -> (a = ), (= a b c) -> ((a = b) and (b = c))."
  [form]
  (let [op (first form)
        args (rest form)
        two-arg-form #(list (first %) op (second %))]
    (cond (< (count args) 2)
            (throw (Exception. (format "need at least 2 arguments for %s" op)))
          (= (count args) 2)
            (list (first args) op (second args))
          :else
            (interpose 'and (map #(list % op %2) args (rest args))))))

(def #^{:doc "Mapping from function symbols to infix expansion fns."}
     sql-condition-ops
  (let [make-op-map (fn [op-expand-fn ops] (into {} (map #(vector %, op-expand-fn) ops)))]
    (merge (make-op-map clj->sql-2arg-op '(= < > not=))
           (make-op-map clj->sql-multiarg-op '(and or)))))

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
                                         #(cons 'list (cons (list 'quote (first %)) (rest %)))
                                         e))
        quote-fields (fn [e] (walk-expr cut-on-quote
                                        #(field-name %)
                                        #(list 'quote %)
                                        e))]
    (quote-fields (quote-sql-ops expr))))

(defmacro condition
  "create a function which creates a condition function given
  a vector of columnnames of a particular relation.
  Calling the constructor without an arg will return the underlying
  condition-expression."
  ([expr] `(condition ~'anonymous-condition ~expr))
  ([condition-name expr]
  (let [used-fields (collect-exprs field-name expr)
        used-field-names (map field-name used-fields)
        fields (gensym)
        tuple (gensym)
        position-vars (take (count used-field-names) (repeatedly gensym))
        column-lookup-expansion (fn [field] `(pos ~fields '~field))]
    `(fn ~(symbol (str condition-name "-ctor"))
      ([] [~(quote-condition-expression expr), '~used-field-names])
      ([~fields]
        (let ~(vec (mapcat #(list % (column-lookup-expansion %2)) position-vars used-field-names))
          (fn ~(symbol (str condition-name)) [~tuple]
            (let ~(vec (mapcat #(list %1 (list tuple %2)) used-fields position-vars))
              ~expr))))))))

;; relational algebra operations

(defn op-dispatch-fn [relation & rest]
  (:relation-tag ^relation))

(defn two-op-dispatch-fn [R, S & rest]
  (let [tag-r (:relation-tag ^R)
        tag-s (:relation-tag ^S)]
    (or (or (nil? tag-r) (nil? tag-s))
        (and (= tag-r tag-s) tag-r)
        :clojure)))

(defmulti project op-dispatch-fn)
(defmulti select op-dispatch-fn)
(defmulti rename op-dispatch-fn)
(defmulti xproduct two-op-dispatch-fn)
(defmulti join two-op-dispatch-fn)
(defmulti union two-op-dispatch-fn)
(defmulti difference two-op-dispatch-fn)
(defmulti intersection two-op-dispatch-fn)

;; need: merge or pkey-aware union!!

;; default dispatch
(defmacro def-default-method
  "Expands to a defmethod form which defines the default dispatch method (on true) for 
  the given rel-algebra method name taking R-arg-count num args."
  [name R-arg-count]
  (let [rel-args (repeatedly gensym)]
    `(defmethod ~name true [~@(take R-arg-count rel-args) & args#]
       (apply ~'project ~@(map (fn [argname] `(make-relation ~argname)) (take R-arg-count rel-args)) args#))))

(comment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;"primary keys" in relations:
; internal: "primary index"
; how to calculate the union with other sets?
#{[a 1] [b 2]} U #{[b 3]} -> #{[a 1] [b 2] [b 3]}
; but with #0 as primary-key
#{[a 1] [b 2]} U #{[b 3]} -> either #{[a 1] [b 2]} or #{[a 1] [b 3]} ??  ;-> merge ?

; consequence of pkey: 
[b 1] == [b 2] == [b ?x]
; whereas without
[b 1] != [b 2] != [b ?X]
; NO!

; affects: all ops except select
;project: remove the pkey tag if pkey field is not projected
;rename: rename field attr map too
;xproduct, join:  add new pkey if S has pkey
;set-ops: work only on the `primrary' key

; mhh, constraints (pkeys here) do really matter only on "inserts"
; rel. operations don't need them (yes, maybe for optimizing but thats beyond the scope of my impl.)

; first try: field attributes in metadata
; ex: (make-relation hoeck.rel.test/people-literal :fields '[#^{:primary-key true} id name vorname address-id])

; other approaches: (make-relation bla :fields '[id ...] :primary-key [id])

(make-constrained-relation ...)
;-> internal: {id [name city]}
;external: implements clojure.lang.IPersistentSet or hoeck.rel.Relation

;pseudo-clojure:
;(let [data '{1 [franz kafka], 
;             2 [frank codd],
;             3 [erich kaestner]}
;      index-fn (fn [] (if `index-of-id-wanted') data, normal index on "set" of data)]
;(fproxy-relation `[meta :index index-fn :tag clojure-constrained-relation]'
;  {'seq (fn [] (map #(vector `(val %) (key %) (val %)') data))
;   'count (fn [] (count data))
;   'contains (fn [tup] (if (data `id-extracted-from-tup') true false))
;   'get (fn [tup] like^^^^^)})

; [. * .]
; [* * *] <- selection
; [. * .]
;    ^
;    `------ projection
; =====================
; [*]


; [* * *] [* *]
; [* * *] [* *] <- join ^= concat of tuples
; [* * *] [* *]
; =====================
; [* * * * *]
; [* * * * *]
; [* * * * *]


; [* * *]
; [* * *]
;  U,D,I
; [* * *]
; [* * *]
; ======= set operation ^= concat of rows
; [* * *]
; [* * *]
; [* * *]


)
