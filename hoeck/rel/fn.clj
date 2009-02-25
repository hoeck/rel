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

(defn simple-projection
  "A projection without any expressions other than field-names."
  ([R names] (simple-projection R names true)) ;; undecided about align yet
  ([R names align?]
  (let [fields (fields R)
        index-fn (index R)
        sig (generate-signature fields names);; signature: number -> number lookup
        align? (or align? (some nil? sig)) ; align if new fields are introduced
        sig (if align? sig (vec (sort sig)))]
    (cond (empty? sig) nil
          (= sig [nil]) (empty-relation names)
          :else (let [project-tuple-fn (if align?
                                         #(reduce (fn [ret cur] (conj ret (if cur (nth % cur)))) [] sig)
                                         #(subnvec % sig))
                             
                      m {:index (project-index R sig project-tuple-fn)
                         :fields (vec (map fields sig))}
                      
                      seq-fn (fn simple-projection-seq [_] (seq (map project-tuple-fn (seq R))))
                      count-fn (fn simple-projection-count [this] (count R))
                      get-fn (fn simple-projection-get [_ key] (if (multi-index-lookup index-fn sig key) key))]
                  (fproxy-relation (merge (meta R) m) 
                                   {'seq seq-fn,
                                    'count count-fn,
                                    'contains default-contains-fn
                                    'get get-fn}))))))

(def sample-project-expression
     (list
      ;; either a symbol/keyword denoting a field
      'name
      ;; or a condition-function
      (condition (* *id *address-id))
      ;; or a pair of one of the former expr plus the new field name
      [(condition (str *name ", " *vorname)) 'name-string]))

(defn parse-projection-expression
  "Return a list of [condition new-field-name] pairs out of a
  projection expression."
  [expr]
  (map #(cond (vector? %)
                %                                    
              (or (symbol? %) (keyword? %))
                [(make-identity-condition %) %]
              :else
                ;; if no new name  given, use field1-field2-...-fieldn as new fieldname
                [% (symbol (apply str (interpose "-" (:fields (%)))))])
       expr))

(defn advanced-projection
  "A project which allows expressions as projections"
  [R expr]
  ; 1) get the list of expressions or symbols
  ; 2) construct 3 vectors (as in simple-project)
  ;    input-fields functions new-field-names
  ; 3) create a function: tuple -> projected-tuple
  ; 4) write the (lazy) projection implementation
  ; 5 think about how to implement an index over the projected values
  ;   (is this even possible on projected expressions?)
  (let [fields (fields R)
        input-fields (map #(:fields ((first %))) expr)
        input-fields-pos (map #(map (partial pos fields) %) input-fields)
        functions (map #((first %1) %2) expr input-fields)
        new-field-names (let [fnames-wo-meta (vec (map second expr))
                              fset (set fields)]
                          (vec (map #(with-meta % (meta (fset %))) fnames-wo-meta)))

        project-tuple (fn [tup] (vec (map #(%2 (subnvec tup %1))
                                          input-fields-pos functions)))

        ;; for now, enable index lookup only in the simple-projected fields
        ;; (the fields projected with the identity-function)
        lookupable-fields (set (map first (map :fields (filter #(= :identity (:type %)) (map #((first %)) expr)))))
        ;; positions of the identity fields in the tuples of R, nil if field is not lookupable
        lookup-fields-pos (vec (map #(if (and (lookupable-fields (first %1)) (not (next %1))) (first %2) nil) input-fields input-fields-pos))
        
        ;; ideally, we need the inverse function of project-tuple for index-projection and tuple lookup
        ;; practically, we use only the identity-projected fields to built the
        ;; new index and recalculate the complex-projected ones
        unproject-tuple (fn [ptup]
                          '???)

        indexR (index R)        

        set-data (delay (set (map project-tuple R)))

        seq-fn (fn seq-fn [_] (seq (force set-data)))
        count-fn (fn count-fn [_] (count R))
        get-fn (fn get-fn [_ tup]
                 (if (not (empty? lookupable-fields))
                   (let [idx (multi-index-lookup indexR (filter identity lookup-fields-pos) (subnvec tup (filter identity lookup-fields-pos)))]
                     (some #(= tup %) (map project-tuple idx)))
                   ((force set-data) tup)))
        
        projected-R (fproxy-relation (merge (meta R) {:fields new-field-names})
                                     {'seq seq-fn
                                      'count count-fn
                                      'get get-fn
                                      'contains default-contains-fn
                                      })
        
        index-fn (fn index-fn
                   ([p] 
                      ;; is p in lookupable-fields?                      
                      ;; (yes) get idx of p, map all tuples with project-tuple
                      ;; (no) create index on p in projected relation
                      (if-let [orig-pos (lookup-fields-pos p)]
                        (into {} (map (fn [[k v]] [k (set (map project-tuple v))]) (indexR orig-pos)))
                        (make-index-on-set projected-R #(nth % p))))
                   ([p v]
                      (if-let [orig-pos (lookup-fields-pos p)]
                        (set (map project-tuple (indexR orig-pos v)))
                        (set (filter #(= (nth % p) v) projected-R)))))
        ]
    (with-meta projected-R (merge (meta projected-R) {:index index-fn}))))


(defmethod project :clojure
  ([_] (empty-relation))
  ([R exprs] 
     ;; an expr is either a (hoeck.rel.core/condition ..) or a symbol or a keyword 
     ;; or a [hoeck.rel.core/condition (or symbol keyword)]
     (cond (= (fields R) exprs) 
             R ; identity
           (every? #(or (symbol? %) (keyword? %)) exprs)
             (simple-projection R exprs)
           :else
             (advanced-projection R (parse-projection-expression exprs)))))


;;; selection:

  "Filter data from a relation R based on condition.
  Condition is a function that returns :signature or :condition"
(defmethod select :clojure
  [R condition-ctor]
  (let [fields (fields R)
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
        new-fields (map #(or (if-let [r (rename %)] (with-meta r (meta %))) %) (fields R))]
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



(defn make-set-compatible-fields
  ""
  [fields-R fields-S]
  (let [cR (count fields-R)
        cS (count fields-S)]
    (cond (< cS cR)
            fields-R
          (<= cR cS)
            fields-S)))

(defmethod union :clojure
  ([R S]     
     (let [;[R, S] (let [f (make-set-compatible-fields (fields R) (fields S))]
           ;         [(project R f), (project S f)])
                      
           indexR (index R), indexS (index S)
           
           [conR conS] (map constraints [R S])
             
           primary-key-fields (:primary-key (or conR conS))
           primary-key (empty?->nil (vec (map #(pos (fields (if conR R S)) %) primary-key-fields)))

           ; = for 2 tuples, taking a primary key into account
           tupl= (if primary-key 
                   (fn [a b] (every? true? (map #(= (a %) (b %)) primary-key)))
                   =)
           
           extract-pkey (if primary-key
                          (fn [tup] (subnvec tup primary-key))
                          identity)

           ; lookup in relation S taking pkey into account
           containsS (if primary-key
                       (if-let [p (single? primary-key)]
                         (fn [tup] (empty?->nil (indexS p (nth tup p))))
                         (fn [tup] (empty?->nil (multi-index-lookup indexS primary-key (extract-pkey tup)))))
                       S)
           
           pkey-union-seq (fn [r s] (lazy-cat s (remove containsS r)))
           
           seq-fn (fn [_] (pkey-union-seq R S))
           count-fn (fn [this] (.count (.seq this)))
           get-fn (fn [_ key] (or (S key) (and (not (containsS key)) (R key))))
           contains-fn (fn [this key] (if (.get this key) true false))

           index-fn (fn ([n] 
                           ; for each value in index s, 
                           ; look if there is a value in index r
                           ; if so do a union r,s
                           ; add all keys to index which contents are not in S (via containsS)
                           (into {} (concat (map (fn [[k s]]
                                                   [k
                                                    (if-let [r ((indexR n) k)]
                                                      (set (pkey-union-seq r s))
                                                      s)])
                                                 (indexS n))
                                            (remove nil?
                                                    (map (fn [[k r]]
                                                           (if-not ((indexS n) k)
                                                             [k (empty?->nil (set (remove containsS r)))]))
                                                         (indexR n))))))
                        ([n v]
                           (set (let [r (indexR n v)
                                      s (indexS n v)]
                                  (pkey-union-seq r s)))))
           ]
       (fproxy-relation (merge (meta R) {:index index-fn})
         {'seq seq-fn
          'count count-fn
          'get get-fn
          'contains contains-fn}))))

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

