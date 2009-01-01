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

;;; relational algebra on clojure sets

(ns hoeck.rel.fn
  (:require [clojure.set :as set])
  (:use hoeck.rel.core hoeck.library clojure.contrib.fcase)
  (:import (clojure.lang MapEntry)))

(defn empty-relation [fields]
  (hoeck.rel.core/make-relation #{} :fields (vec fields)))

;;; projection:

(defn generate-signature
  "Generate a vector mapping old to new positions in a projected tuple."
  [old-names, new-names]
  (vec (map #(pos old-names %) new-names)))
;  (let [new-names (set new-names)]
;    (vec (map #(when (new-names %) %2) old-names (range 0 (count old-names))))))

(defn project-index
  "Return a new projected index-function."
  [R fields-sig project-tuple-fn]
  (let [index-fn (index R)]
    (fn ([n] (into {} (map (fn [e] [(key e), (set (map project-tuple-fn (val e)))]) (if-let [p (nth fields-sig n)] (index-fn p) {nil R}))))
        ([n v] (if-let [p (nth fields-sig n)] (set (map project-tuple-fn (index-fn p v))) (if (nil? v) R))))))

(defmethod project :clojure
  ([_] (empty-relation))
  ([R names] (project R names false))
  ([R names align?] ;; align: reorder tuple-vectors; not compatible with relational algebra; question: why not use struct-maps???
     (if (= (fields R) names) R ;; identity
         (let [fields (fields R)
               index-fn (index R)
               sig (generate-signature fields names) ;; signature: number -> number lookup
               align? (or align? (some nil? sig)) ; align if new fields are introduced
               sig (if align? sig (vec (sort sig)))]
           (cond (empty? sig) nil
                 (= sig [nil]) (empty-relation names)
                 :else (let [project-tuple-fn (if-let [p (single? sig)]
                                                #(nth % p)
                                                (if align?
                                                  #(reduce (fn [ret cur] (conj ret (if cur (nth % cur)))) [] sig)
                                                  #(subnvec % sig)))
                             
                             m {:index (project-index R sig project-tuple-fn)
                                :fields (vec (if align? names (map fields sig)))}
                             
                             seq-fn (fn [_] (map project-tuple-fn (seq R)))
                             count-fn (fn [this] (count R))
                             get-fn (fn [_ key] (if (multi-index-lookup index-fn sig key) key))
                             contains-fn (fn [this key] (if (.get this key) true false))]
                         (fproxy-relation (merge (meta R) m) 
                           {'seq seq-fn,
                            'count count-fn,
                            'contains contains-fn,
                            'get get-fn })))))))

;;; selection:

  "Filter data from a relation R based on condition.
  Condition is a function that returns :signature or :condition"
(defmethod select :clojure
  [R condition-ctor]
  (let [;;[expr, used-fields] (condition-ctor)
        fields (fields R)
        index-fn (index R)
        condition-fn (condition-ctor fields)

        seq-fn (fn [_] (filter condition-fn R))
        count-fn (fn [this] (.count (.seq this)))
        get-fn (fn [_ key] (and (contains? R key) (condition-fn key)))
        contains-fn (fn [this key] (if (.get this key) true false))
        
        m {:index (fn ([n] (reduce (fn [m e] (if-let [r (empty?->nil (set (filter condition-fn (val e))))]
                                               (assoc m (key e) r)
                                               m))
                                   {} (index-fn n)))
                      ([n v] (empty?->nil (set (filter condition-fn (index-fn n v))))))}]

  (fproxy-relation (merge (meta R) m)
    {'seq seq-fn
     'count count-fn
     'get get-fn
     'contains contains-fn})))

;;; rename: 

(defmethod rename :clojure
  [R name-newname-pairs]
  (let [rename (apply hash-map name-newname-pairs)
        new-fields (map #(or (rename %) %) (fields R))]
    (with-meta R
      (merge (meta R)
             {:fields (vec new-fields)}))))

;;; cross-product:

"Return the cross-product of two relations R and S."
(defmethod xproduct :clojure
  [R S]
  (let [indexR (index R), indexS (index S)
        field-count-R (-> R fields count)

        seq-fn (fn [_] (mapcat (fn [r] (map (fn [s] (into-tuple r s)) (seq S))) (seq R)))
        count-fn (fn [this] (* (count R) (count S)))
        get-fn (fn [_ key] (and (.get R (subvec key 0 field-count-R)) (.get S (subvec key field-count-R)) key))
        contains-fn (fn [this key] (if (.get this key) true false))

        m {:fields (vec (concat (fields R) (fields S)))
           :index (fn xproduct-index
                    ([n] (into {} (map #(vector % (xproduct-index n %)) (keys (if (< n field-count-R) (indexR n) (indexS (- n field-count-R)))))))
                    ([n v] (empty?->nil
                            (let [indexR? (< n field-count-R)]
                              (set (mapcat (if indexR? (fn [r] (map #(into-tuple r %) S))
                                                       (fn [s] (map #(into-tuple % s) R)))
                                           (if indexR? (indexR n v)
                                                       (indexS (- n field-count-R) v))))))))}]

    (fproxy-relation (merge (meta S) (meta R) m)
      {'seq seq-fn
       'count count-fn
       'get get-fn
       'contains contains-fn})))


;;; join:
"equi-join collumns r and s of R and S using the index of S."
;;; better error messages if column is not in relation
(defmethod join :clojure
   [R S r s]
   (let [indexS (index S), indexR (index R), s-pos (pos (fields S) s), r-pos (pos (fields R) r)
         field-count-R (-> R fields count)

         join-tuple (fn [r-tup]
                       (let [r-val (r-tup r-pos)
                             s-tuples (indexS s-pos r-val)]
                         (if-let [s-tup (single? s-tuples)]
                           (list (into-tuple r-tup s-tup))
                          (map #(into-tuple r-tup %) s-tuples))))

         seq-fn (fn seq-fn [_] (mapcat join-tuple R))
         count-fn (fn count-fn [this] (.count (.seq this)))
         get-fn (fn get-fn [_ key]
                  (if-let [key-r (subvec key 0 field-count-R)]
                    (if-let [key-s (subvec key field-count-R)]
                      (if (= (key-r r-pos) (key-s s-pos)) key))))
         contains-fn (fn [this key]
                       (if (.get this key) true false))

         m {:fields (vec (concat (fields R) (fields S)))
            :index (fn join-index
                     ([n] (into {} (map #(vector % (join-index n %)) (keys (if (< n field-count-R) (indexR n) (indexS (- n field-count-R)))))))
                     ([n v] (empty?->nil (set (let [indexR? (< n field-count-R)]
                                               (if indexR? (mapcat join-tuple (indexR n v))
                                                           (mapcat (fn [S-tup] (map (fn [R-tup] (into-tuple R-tup S-tup)) (indexR r-pos (S-tup s-pos)))) (indexS (- n field-count-R) v))))))))                                              
                              }]
     (fproxy-relation (merge (meta S) (meta R) m)
       {'seq seq-fn
        'count count-fn
        'get get-fn
        'contains contains-fn})))

;;; union {id c-id name c-name}
(defmethod union :clojure
  ([R S] 
     (let [seq-fn (fn [_] (lazy-cat R (filter (complement R) S)))
           count-fn (fn [this] (.count (.seq this)))
           get-fn (fn [_ key] (or (R key) (S key)))
           contains-fn (fn [this key] (if (.get this key) true false))

           indexR (index R), indexS (index S)
           
           m {:index (fn ([n] (let [indexS-at-n (indexS n)
                                    indexR-at-n (indexR n)]
                                (into {} 
                                      (concat (map #(vector (key %) (set/union (indexS-at-n (key %)) (val %))) indexR-at-n)
                                              (filter #(not (contains? indexR-at-n (key %))) indexS-at-n)))))
                         ([n v] (set/union (indexR n v) (indexS n v))))}
           ]
       (fproxy-relation (merge (meta R) m)
         {'seq seq-fn
          'count count-fn
          'get get-fn
          'contains contains-fn}))))

(set/intersection #{:a 3 :c} #{1 :a 3})

(defmethod difference :clojure
  [R S]
  (let [seq-fn (fn [_] (filter (complement S) R))
        count-fn (fn [this] (.count (.seq this)))
        get-fn (fn [_ key] (if (R key) (and (not (S key)) key)))
        contains-fn (fn [this key] (if (.get this key) true false))

        indexR (index R), indexS (index S)

        m {:index (fn ([n] (into {} (filter identity (map (let [indexS-at-n (indexS n)]
                                                            #(if (not (indexS-at-n (key %))) (vector (key %) (set (filter (complement S) (val %))))))
                                                          (indexR n)))))
                      ([n v] (set (filter (complement S) (indexR n v)))))}
        ]
    (fproxy-relation (merge (meta R) m)
      {'seq seq-fn
       'count count-fn
       'get get-fn
       'contains contains-fn})))

;; from set.clj
(defmethod intersection :clojure
  [R S]
  (difference R (difference R S)))





