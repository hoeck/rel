
;; eager evaluating implementation of the relational operators

(ns hoeck.rel.non-lazy
  (:use hoeck.rel.operators
        hoeck.library
        clojure.contrib.pprint
	clojure.contrib.except)
  (:require [clojure.set :as set]
            [hoeck.rel.expressions :as e]))

(use 'hoeck.rel.testdata)

(defmethod relation clojure.lang.IPersistentVector
  ;; (relation [:name :path] [:c :d :e :f]) -> #{{:name :c :path :d} ..}
  ;; (relation [:name :path] [1 2 3] [4 5 6]) -> #{{:name 1 :path 4} {:name 2 :path 5} {:name 3 :path 6}}
  ;; (relation [[:name :path]] [[:a :b] [:x :y]]) -> #{{:name :a :path :b} ..}
  [rdef & data]
  (cond (vector? (first rdef))
	  (if (next data)
	    (throw-arg "%s clause expects ONE seq of nested seqs, not multiple seqs.")
	    (let [fields (first rdef)]
	      (with-meta (->> data
			      first
			      (map (partial zipmap fields))
			      set)
			 {:fields fields})))
        :else
	  (if (next data)
	    (set (apply map #(zipmap rdef %&) data))
	    (relation (vector rdef) (partition (count rdef) (first data))))))

(defmethod relation clojure.lang.Keyword
  ;; (relation :name [:c :d :e :f]) -> #{{:name :c} {:name :a} ..}
  ;; (relation :name [:c :d :e :f] :value [1 2 3 4]) -> #{{:name :c :value 1} ..}
  [& rdef]
  (let [[keys values] (deinterleave rdef)]
    (apply relation (vec keys) values)))

(defmethod relation clojure.lang.IPersistentSet
  [s] s) ;; identity

;; op

(defmethod project :clojure
  ([_] #{})
  ([R exprs]
     (let [prj-fields (map e/get-name exprs)
           exprs (map #(if (e/identity? %) 
                         %2
                         (e/get-expr %1))
                      exprs prj-fields)]
       (set (map (fn [tuple]
                   (zipmap prj-fields
                           (map #(% tuple) exprs)))
                 R)))))

(defmethod select :clojure
  ([R expr]
     (set (filter expr R))))

(defn rename-keys [m kmap]
  (if m (into (empty m) (map (fn [[k v]] [(or (kmap k) k), v]) m))))

(defmethod rename :clojure
  ([R rename-map]
     (set (map #(rename-keys % rename-map)
               R))))

(defmethod xproduct :clojure
  [R S]
  (set (mapcat #(map (partial merge %) S) R)))

(defmethod fjoin :clojure
  ;; f is a function which, given a tuple of R, returns a seq of new tuples or nil
  [R f]
  (set (mapcat #(map (partial merge %) (f %)) R)))

(defmethod join :clojure
  ([R S join-expr]
     (set (remove nil? (mapcat (fn [r-tuple]
                                 (map #(when (join-expr r-tuple %)
                                         (merge r-tuple %))
                                      S))
                               R)))))

(defn join= [field-a field-b] #(= (field-a %1) (field-b %2)))

(comment (= (join people address #(= (:adress-id %1) (:id %2)))
            (join people address (join= :adress-id :id))))

(defmethod outer-join :clojure
  ([R S join-fn]
     (if (empty? S)
       R
       (fjoin R (fn [r-tuple] 
                  (map #(if (join-fn r-tuple %)
                          (merge r-tuple %)
                          r-tuple)
                       S))))))

(defmethod union        :clojure [& rels] (apply set/union rels))
(defmethod difference   :clojure [& rels] (apply set/difference rels))
(defmethod intersection :clojure [& rels] (apply set/intersection rels))

;;;; helpers for aggregate
;;(defn get-aggregate-conditions
;;  "Given some conditions, return all aggregate-conditions."
;;  [conditions]
;;  (filter #(= (condition-meta % :type) :aggregate) conditions))

;; some predefined aggregate functions
(defn a-sum [field] (fn [R] {field (apply + (map field R))}))
(defn a-avg [field] (fn [R] {field (/ (reduce + (map field R)) (count R))}))
(defn a-count [field] (fn [R] ({field (count R)})))
(defn a-min [field] (fn [R] {field (apply min (map field R))}))
(defn a-max [field] (fn [R] {field (apply max (map field R))}))

(defmethod aggregate :clojure
  [R exprs] ;; identity-exprs and or aggregate-expr
  (let [aggregates (remove e/identity? exprs)
        groups (->>  exprs
                     (filter e/identity?)
                     (map e/get-name))
        agg-names (map e/get-name aggregates)
        index (set/index R groups)]
    (set (map (fn [[k v]]
                (apply merge k (map #((e/get-expr %) v) aggregates)))
              index))))

(comment (aggregate agg-test-rel [(e/identity-expr :group)
                                  (a-sum :power)]))

(defmethod order-by :clojure [R fields-or-fn]
  (if (fn? (first fields-or-fn))
    (apply sorted-set-by fields-or-fn R)
    (throwf "todo")))