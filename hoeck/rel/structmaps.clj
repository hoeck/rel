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
  (:require clojure.set
            [hoeck.rel.testdata :as testdata])
  (:use (hoeck library magic-map value-mapped-map)
        (hoeck.rel operators conditions)
        [hoeck.rel.testdata :only [with-testdata]]
        de.kotka.lazymap
        ;; cl-format, pprint
        clojure.contrib.test-is)
  (:import (hoeck.rel Relation)))

;; type aware select-keys
(defn select-keys
  "Returns a map of the same type as map containing only those entries in map
  whose key is in keys"
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

(def empty-relation)

(defn fields [R] (:fields ^R))
(defn index [R] (:index ^R))

;(def people (with-meta people-literal
;                       {:relation-tag :clojure
;                        :fields #{:id :name :vorname :adress-id}}))

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

(defmethod make-index clojure.lang.IPersistentSet [R fields & opts]
  (let [;; lazy index
        index-map (apply lazy-hash-map* (mapcat #(list %  (delay (set-index R %))) fields))
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

;(def people (with-meta people (merge ^people {:index (make-index people)})))

(defmethod make-relation clojure.lang.IPersistentSet
  [S & opts]
  (let [o (apply hash-map opts)
        f (or (o :fields) (keys (first S)))]
    (with-meta S (merge ^S {:relation-tag :clojure
                            :fields f
                            :index (make-index S f)}))))

(deftest make-relation-test
  (with-testdata
    (is (= (set (fields R)) testdata/fields-R))
    (is (= (set (fields S)) testdata/fields-S))
    (is (= R testdata/people))
    (is (= (index R) testdata/index-R))
    (is (= S testdata/address))
    (is (= (index S) testdata/index-S))))

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
        get-fn (fn get-fn [this tup] (first (index-lookup new-index tup)))]
    (Relation. (merge (meta R) {:fields field-names
                                :index new-index})
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
           get-fn (fn [this tup] (first (index-lookup new-index tup)))]
       (Relation. (merge ^R {:fields (concat (fields R) new-field)
                             :index new-index})
                  {'seq seq-fn
                   'count count-fn
                   'get get-fn})))
  ([R expr & exprs]
     (project-expression (apply project-expression R exprs) expr)))

;; example;
;;(project-expression (make-relation testdata/people) (condition (* ~id 10)) (condition (- ~id 100)) (condition (+ ~id 1000)))

(defmethod project :clojure [R exprs]
  (let [exprs (read-expressions exprs)
        identity-expr? #(= (:type (condition-meta %)) :identity)
        complex-exprs (filter (complement identity-expr?) exprs)
        all-projected-fields (map #(:name (condition-meta %)) exprs)]
    (project-identity (if (seq complex-exprs)
                        (apply project-expression R complex-exprs)
                        R)
                      all-projected-fields)))

;; example: (project (make-relation testdata/people) (list :id (condition (str ~vorname "-" ~id)) :name))

(deftest project-identity-test ;; identity == remove cols only
  (with-testdata
    (let [p (project R '(:vorname :id))]
      (is (= (set (fields p)) #{:vorname :id}) "fields")
      (is (= (set (fields p)) (set (keys (first p)))) "fields and first tuple")
      (is (= p (project R '(:id :vorname))) "expression ordering")
      (is (= (get-in (index p) '(:vorname robert)) '#{{:id 4, :vorname robert} {:id 3, :vorname robert}}) "index")
      (is (= (index p) (make-index p (fields p))) "index")
      (is (= (project p '(:vorname)) (project (project (project R '(:vorname :name :id)) '(:vorname :id)) '(:vorname))) "nesting")
      (is (= #{} (project p ())) "empty projection"))))

(deftest project-expr-test
  (with-testdata
    (let [expr (list :id (condition (str ~vorname "-" ~id) :name-id) :name)
          p (project R expr)]
      (is (= (map #(str (:vorname %) "-" (:id %)) R) (map :name-id p)) "expression")
      (is (= (index p) (make-index p (fields p))) "index"))))

(defmethod select :clojure [R expr]
  (let [new-index (filter-index expr (index R))
        seq-fn (fn [_] (distinct (filter expr R)))
        get-fn (fn [_ k] (first (index-lookup new-index k)))
        count-fn (fn [_] (count (seq-fn _)))]
    (Relation. (merge (meta R) {:index new-index})
               {'seq seq-fn
                'count count-fn
                'get get-fn})))

;;; example: (select (project people '(:id :name)) (condition (< ~id 4)))

(deftest select-test
  (with-testdata
    (let [s (select R (condition (or (= ~id 3) (= ~id 4))))]
      (is (= (count s) 2) "count")
      ;(is (= (index s) (make-index s (fields s))) "index")
      (is (= s (select R (condition (and (< -100 ~id) (< ~id 5) ((complement #{-1 0 1 2}) ~id))))) "equality")
      (is (= (select R (condition ~id)) R) "equality")
      (is (= s (select (select R (condition (< 2 ~id 5))) (condition (#{3 4} ~id)))) "nesting"))))


(defn lazily-rename-keys
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
    (Relation. (merge (meta R) {:fields (map #(kmap % %) (fields R))
                                :index new-index})
               {'seq (fn [_] (seq (clojure.set/rename R kmap)))
                'count (fn [_] (count R))
                'get (fn [_ k] (first (index-lookup new-index k)))})))

;;; example: (index (rename (select (project people '(:id :name)) (condition (< ~id 4))) {:id :ident-number}))

(deftest rename-test
  (with-testdata
   (let [rename-map {:id :pers-id, :vorname :first-name}
         r (rename R rename-map)]
     (is (= (count r) (count R)) "same length")
     (is (= (clojure.set/difference (set (keys (first r))) (set (fields R))) #{:pers-id :first-name}) "keys renamed")
     (is (= (clojure.set/difference (set (fields r)) (set (fields R))) #{:pers-id :first-name}) "fields renamed")
     (is (= (clojure.set/difference (set (keys (first r))) (set (vals rename-map))) #{:name :adress-id}) "originial keys")
     (is (= (clojure.set/difference (set (fields r)) (set (vals rename-map))) #{:name :adress-id}) "original fields")
     (is (= (index r) (make-index r (fields r))) "index"))))

(defn lazy-merge
  "given two hashmaps a and b, merge those lazily."
  [a b]
  (magic-map a (fn ([] (keys b))
                   ([k] (get b k)))))

(defmethod join :clojure [R r S s]
  (let [index-Ss (find (index S) s)
        join-tuple (fn [r-tup] (let [friend (first ((val index-Ss) (r-tup r)))]
                                 (if friend (merge r-tup (dissoc friend s)))))
        reverse-join (fn [s-tup] (map join-tuple (((index R) r) (s-tup s))))
        new-index (lazy-merge (map-index (fn [tuples] (set (filter identity (map join-tuple tuples)))) (index R))
                              (map-index #(set (mapcat reverse-join %)) (dissoc (index S) s)))]
    (Relation. (merge ^S ^R
                      {:fields (concat (fields R) (filter #(not= s %) (fields S))), 
                       :index new-index})
               {'seq (fn [_] (filter identity (map join-tuple R)))
                'count (fn [_] (count R))
                'get (fn [_ k] (first (index-lookup new-index k)))})))

(deftest join-test
  (with-testdata
   (let [j (join R :adress-id S :id)]
     (is (= (set (fields j)) (set (concat (fields R) (filter #(not= :id %) (fields S))))) "joined fields")
     ;(is (= (index j) (make-index j (fields j))) "index")
     )))


;;; two macros for writing set-operations:
(defmacro modify-index-for-set
  "uses R,S as the surrounding Relation vars, and binds r and s to the current index of R and S.
  Captures: r and s, uses R and S."
  {:private :true}
  [magic-map-fn]
  (let [;; index accessors, must be lazy
        index-R `(index ~'R)
        index-S `(index ~'S)]
    ;; implement operations on relation index by creating a nested magicmap which delegates the 
    ;; entry-retrieval to the indexes of R and S
    `(magic-map (fn ([] (distinct (concat (keys ~index-R) (keys ~index-S)))) ;; seq of fields the index contains
                    ([k#] (let [~'r (get ~index-R k#), ~'s (get ~index-S k#)] ;; the contents of a specific field index
                            (magic-map ~magic-map-fn))))))) ;; combine r and s to compute a resulting index

(defmacro def-set-operator
  "Captures R and S"
  {:private true}
  [op-name, index-map-fn, seq-fn, get-fn]
  (let [R 'R, S 'S]
    `(defmethod ~op-name :clojure [~R ~S]
       (Relation. (merge ^~S ^~R {:index (modify-index-for-set ~index-map-fn)
                                  :fields (distinct (concat (fields ~S) (fields ~R)))})
                  {'~'seq ~seq-fn
                   '~'get ~get-fn
                   '~'count (fn [this#] (count (seq this#)))}))))

(def-set-operator union ;; given 2 relations R and S
  (fn ([] (distinct (concat (keys r) (keys s))))
      ([k] (clojure.set/union (get r k) (get s k))))
  (fn [_] (lazy-cat R (filter (complement R) S))) ;; seq
  (fn [_ k] (or (R k) (S k)))) ;; get
  
(def-set-operator difference
  (fn ([] (distinct (concat (keys r) (keys s))))
      ([k] (clojure.set/difference (get r k) (get s k))))
  (fn [_] (filter (complement S) R)) ;; seq
  (fn [_ k] (and (not (S k)) (R k)))) ;; get

(def-set-operator intersection
  (fn ([] (distinct (concat (keys r) (keys s))))
      ([k] (clojure.set/intersection (get r k) (get s k))))
  (fn [_] (distinct (lazy-cat (filter S R) (filter R S)))) ;; seq
  (fn [_ k] (and (S k) (R k)))) ;; get



(deftest union-test
  (let [R (make-relation #{{:a 1 :b 1} {:a 2 :b 1}})
        S (make-relation #{{:a 3 :b 1}})
        u (union R S)]
    (is (= u (set (concat R S))) "union")
    (is (= (index u) (make-index u '(:a :b))) "index")))

