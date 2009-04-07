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
  (:require [clojure.set :as clojure-set])
  (:use hoeck.library 
        hoeck.magicmap 
        hoeck.mapped-map
        hoeck.value-mapped-map
;        hoeck.rel.core       
        hoeck.rel.operators   
        hoeck.rel.conditions  
        clojure.contrib.fcase 
        de.kotka.lazymap      
        com.infolace.format) ;; cl-format, pprint
  (:import (de.kotka.lazymap LazyMap)
           (hoeck.rel Relation)))
                                 

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
        index-map (apply lazy-hash-map* (mapcat #(list % (delay (set-index R %))) (fields R)))
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
  [& expr-or-symbol]
  (map #(if (field? %)
          (make-identity-condition %)
          %)
       expr-or-symbol))

(def example-project-expression
     (read-expressions
      (condition (str ~name ", " ~vorname) :full-name)
      :id))

;; split that into
;;   a `pure' (identity) projection function
;;   a `map' projection function, which adds new, unindexed values to a relation


;(defmethod project :clojure
;  ([_] (empty-relation))
;  ([R exprs]
;     (let [fields (fields R)
;           exprs (apply read-expressions exprs)
;           expr-meta (map #(condition-meta %) exprs)
;           new-fields (map :name expr-meta)
           
;           ;; or: select-keys
;           projected-struct (apply create-struct new-fields)
;           project-tuple (fn [tup] (apply struct projected-struct (map #(% tup) exprs)))
                
;           ;; only those with identity-expressions
;           indexed-fields (pipe expr-meta
;                                (filter #(= (:type %) :identity))
;                                (map :name)
;                                (set))
           
;           ;; two sets
;           [indexed-fields, new-fields] (reduce (fn [[i,n] {:keys [name, type]}]
;                                               (if (= type :identity)
;                                                 [(conj i name), n            ]
;                                                 [i            , (conj n name)]))
;                                             [#{} #{}]
;                                             expr-meta)
        
;           seq-fn (fn seq-fn [_] (distinct (map project-tuple R)))
;           count-fn (fn count-fn [_] (count (seq)))
                
;           new-index-map (apply lazy-hash-map* (mapcat
;                                              #(list % (delay (set-index (seq-fn nil) %)))
;                                              new-fields))

;           new-index (merge new-index-map
;                            (let [old-index (select-keys (index R) indexed-fields)]
;                              (mapped-map
;                               )
;                              (mapped-map (fn ([] ())
;                                          ))
;                              ))

;           (fn index-fn 
;             ([] (pipe (keys (index R))
;                       (filter #(indexed-fields (key (first %))))))
;             ([k] (if (next k)
;                    () 
;                    (single-field-lookup k))))
           
;           get-fn (fn get-fn [this tup] (nnew-index tup))
           
;           projected-R (Relation. (merge (meta R) {:fields new-fields :index new-index})
;                                  {'seq seq-fn
;                                   'count count-fn
;                                   'get get-fn
;                                   })]
;       projected-R)))
(defn map-index [tuple-fn, index]
  ;;{:field {value #{ tuples }}}
  ;;         `---------------´
  ;; `------------------------´
  (value-mapped-map (fn [index-map]
                      (value-mapped-map (fn [tuples] (set (map tuple-fn tuples)))
                                        index-map))
                    index))


(defn project-identity [R field-names]
    ;; just remove some fields, field names must be a list of existing fields
    (let [fields (fields R)

          ;; or: select-keys
          ;;projected-struct (apply create-struct field-names)
          project-tuple (fn [tup] (select-keys tup field-names))

          new-index (map-index project-tuple (select-keys (index R) field-names))                   
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

(defn project-expression [R expr]
    ;; map fields to new fields, an expr is basically a function from field* -> new-field on each `row'
    (let [em (condition-meta expr)
          new-field (:name em)
          project-tuple (fn [tup] (assoc tup new-field (expr tup)))
          new-index (assoc (map-index project-tuple (index R)) new-field (make-index))

          seq-fn (fn [this] (distinct (map project-tuple R)))
          count-fn (fn [this] (count (seq this)))
          get-fn (fn [this tup] (index-lookup new-index tup))]
      (Relation. (merge ^R {:fields nil :index new-index})
                 {'seq seq-fn
                  'count count-fn
                  'get get-fn})))
          
                                           
;(condition-meta (condition (= ~id 10 ~name)))
                                           
                                           
;; project on indices:                     
;;index-map:                               
operator | project     | project-expr    | select | join
---------+-------------+-----------------+--------+---------------------
tuple-fn | select-keys | assoc new field | nil    |   
value-fn | identity    | identity        | nil    |
field-fn | select-keys | assoc new index | nil    |
                                                   
                                                   

 
