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

;;; relational algebra on clojure sets

(ns hoeck.rel.structmaps
  (:refer-clojure :exclude [select-keys])
  (:require [clojure.set :as clojure-set])
  (:use hoeck.library 
        hoeck.magicmap 
        hoeck.mapped-map
        hoeck.value-mapped-map
;        hoeck.rel.core       
        hoeck.rel.operators   
        hoeck.rel.conditions
        de.kotka.lazymap) ;; cl-format, pprint
  (:import (hoeck.rel Relation)))


(def people-literal
     '#{{:id 1 :name weilandt,     :vorname mathias,   :adress-id 100}
        {:id 2 :name kirsch,       :vorname diana      :adress-id 100}
        {:id 3 :name schmock,      :vorname robert     :adress-id 101}
        {:id 4 :name hamann,       :vorname robert     :adress-id 102}
        {:id 5 :name soehnel,      :vorname erik       :adress-id 103}
        {:id 6 :name zschieschang, :vorname mandy      :adress-id 103}
        {:id 7 :name unknown,      :vorname unknown    :adress-id 104}})

(def empty-relation)

(defn fields [R] (:fields ^R))
(defn index [R] (:index ^R))

(def people (with-meta people-literal
                       {:relation-tag :clojure
                        :fields #{:id :name :vorname :adress-id}}))

(defn- set-index
  "Returns a map of the distinct values of k in the xrel mapped to a
  set of the maps in xrel with the corresponding values of k."
  [xrel k]
    (reduce
     (fn [m x]
       (let [ik (x k)]
         (assoc m ik (conj (get m ik #{}) x))))
     {} xrel))

(defn- index-lookup
  "look up multiple fields using a map of single field-indexes"
  ([index-map tuple]
     (empty?->nil
      (apply clojure.set/intersection (map (fn [[k v]] ((index-map k) v)) tuple)))))

(defmethod make-index :clojure [R & opts]
  (let [;; lazy index
        index-map (apply lazy-hash-map* (mapcat #(list %  (delay (set-index R %))) (fields R)))
        ;; index-hashmap, lazy, looks exactly like the index-function in clojure.set
;        index-interface (magicmap (fn 
;                                    ([] (mapcat #(map (partial hash-map %) (keys (index-map %))) (fields R)))
;                                    ([k];; a hashmap {:field val}
;                                       (if (next k)
;                                         (index-lookup index-map k))
;                                       (let [[k,v] (first k)]
;                                         ((index-map k) v)))))
        ]
    index-map))

(def people (with-meta people (merge ^people {:index (make-index people)})))

; magic-map:
;  an object implementing IPersistentMap using
;  (1) a normal map of keys -> values
;  (2) a dispatch function mapping keys (key-ranges) to functions

(defn- field?
  [form]
  (keyword? form))

(defn read-expressions
  "Read a list of symbols or conditions and 
  convert all symbols to identity-conditions returning
  a list of conditions."
  [exprs]
  (map #(if (field? %)
          (make-identity-condition %)
          %)
       exprs))

(def example-project-expression
     (read-expressions
      (list (condition (str ~name ", " ~vorname) :full-name)
            :id)))

;; split that into
;;   a `pure' (identity) projection function
;;   a `map' projection function, which adds new, unindexed values to a relation


(defn map-index [index-fn, index]
  ;;{:field {value #{ tuples }}}
  ;;         `---------------´
  ;; `------------------------´
  (value-mapped-map (fn [index-map]
                      (value-mapped-map index-fn
                                        index-map))
                    index))

(defn map-index-tuples [tuple-fn, index]
  ;;{:field {value #{ tuples }}}
  ;;         `---------------´ index-fn
  ;; `------------------------´ valuemappedmap
  (map-index (fn [tuples] (set (map tuple-fn tuples))) index))

(defn filter-index [tuple-pred, index]
  ;; {field {value #{ tuples }}}
  ;;    |      |        `- filter tuple-pred tuples
  ;;    |      `- empty-set ^= filtered
  ;;    `- identity
  (map-index (fn [tuples] (set (filter tuple-pred tuples))) index))

(defn project-identity 
  "From all tuples of Relation R, remove all tuple-keys not in field-names."
  [R field-names]
  (let [fields (fields R)
        
        ;;projected-struct (apply create-struct field-names)
        project-tuple (fn [tup] (select-keys tup field-names))
        
        new-index (map-index-tuples project-tuple (select-keys (index R) field-names))                   
        ;;{:field {value #{ tuples }}}
        ;;  |        |        |
        ;;  |        |        `remove unprojected from each tuple
        ;;  |        |        
        ;;  |        `identity
        ;;  |
        ;;  `remove-all non-projected
        
        seq-fn (fn seq-fn [_] (distinct (map project-tuple R)))
        count-fn (fn count-fn [this] (count (seq this)))
        get-fn (fn get-fn [this tup] (new-index tup))]
    (Relation. (merge (meta R) {:fields field-names :index new-index})
               {'seq seq-fn
                'count count-fn
                'get get-fn
                })))

(defn project-expression 
  ([R expr]
     ;; map fields to new fields, an expr is basically a function from field* -> new-field on each `row'
     (let [em (condition-meta expr)
           new-field (:name em)
           project-tuple (fn [tup] (assoc tup new-field (expr tup)))

           new-index (lazy-assoc (map-index-tuples project-tuple (index R))
                                 new-field 
                                 (set-index (map project-tuple R) new-field))

           seq-fn (fn [this] (distinct (map project-tuple R)))
           count-fn (fn [this] (count (seq this)))
           get-fn (fn [this tup] (index-lookup new-index tup))]
       (Relation. (merge ^R {:fields (concat (fields R) new-field) :index new-index})
                  {'seq seq-fn
                   'count count-fn
                   'get get-fn})))
  ([R expr & exprs]
     (project-expression (apply project-expression R exprs) expr)))

;; example;
;;(index (project-expression people (condition (* ~id 10)) (condition (- ~id 100)) (condition (+ ~id 1000))))

(defmethod project :clojure [R exprs]
  (let [exprs (read-expressions exprs)
        identity-expr? #(= (:type (condition-meta %)) :identity)
        complex-exprs (filter (complement identity-expr?) exprs)
        all-projected-fields (map #(:name (condition-meta %)) exprs)]
    (project-identity (if (seq complex-exprs) 
                        (apply project-expression R complex-exprs)
                        R)
                      all-projected-fields)))

;; example: (project people (list :id (condition (str ~vorname "-" ~id)) :name))


(defmethod select :clojure [R expr]
  (let [new-index (filter-index expr (index R))
        seq-fn (fn [_] (distinct (filter expr R)))
        get-fn (fn [_ k] (index-lookup new-index k))
        count-fn (fn [_] (count (seq-fn _)))]
    (Relation. (merge (meta R) {:index new-index})
               {'seq seq-fn
                'count count-fn
                'get get-fn})))

;;; example: (select (project people '(:id :name)) (condition (< ~id 4)))

(defn select-keys
  "Returns a map containing only those entries in map whose key is in keys"
  [map keyseq]
    (loop [ret (empty map) keys (seq keyseq)]
      (if keys
        (let [entry (. clojure.lang.RT (find map (first keys)))]
          (recur
           (if entry
             (conj ret entry)
             ret)
           (next keys)))
        ret)))

(defn lazily-rename-keys
  ;; needs clojure.core/select-keys be fixed to use (empty map) instead of just {} !!!
  "Returns the map with the keys in kmap renamed to the vals in kmap.
  Map must be a LazyMap or ValueMappedMap of a LazyMap."
  [map kmap]
  (reduce 
   (fn [m [old new]]
     (if (and (not= old new) (contains? m old))
       (lazy-assoc* (dissoc m old) new (.getRawValue (find m old)))
       m))
   map kmap))

(defmethod rename :clojure [R kmap]
  (let [new-index (map-index-tuples #(clojure.set/rename-keys % kmap) (lazily-rename-keys (index R) kmap))]
    (Relation. (merge (meta R) {:index new-index})
               {'seq (fn [_] (seq (clojure.set/rename R kmap)))
                'count (fn [_] (count R))
                'get (fn [_ k] (index-lookup new-index k))})))

;;; example: (index (rename (select (project people '(:id :name)) (condition (< ~id 4))) {:id :ident-number}))


(defn lazy-merge
  "given lazy hashmaps a and b, merge those without calculating the keys."
  [a b]
  (reduce #(lazy-assoc* %1 (key %2) (.getRawValue %2)) a b))

;; (right) outer-join
(defmethod outer-join :clojure [R r S s]
  (let [index-Ss (find (index S) s)
        join-tuple (fn [r-tup] (merge r-tup (first ((val index-Ss) (r-tup r)))))
        reverse-join (fn [s-tup] (set (map join-tuple (((index R) r) (s-tup s)))))
        new-index (lazy-merge (map-index-tuples join-tuple (index R))
                              (map-index #(set (map reverse-join %)) (index S)))]
    (Relation. (merge ^S ^R
                      {:fields (concat (fields R) (fields S)), 
                       :index new-index})
               {'seq (fn [_] (map join-tuple R))
                'count (fn [_] (count R))
                'get (fn [_ k] (index-lookup new-index k))})))

;; (outer-join people :id (rename (project (select people (condition (< ~id 3))) '(:id :name)) {:id :id2, :name :name2}) :id2)
;;; right-inner-join:
;; (select (outer-join people :id (rename (project (select people (condition (< ~id 3))) '(:id :name)) {:id :id2, :name :name2}) :id2) (condition ~id2))
