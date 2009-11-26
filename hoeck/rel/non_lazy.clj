
;; eager evaluating implementation of the relational operators

(ns hoeck.rel.non-lazy
  (:use hoeck.rel.operators
        hoeck.rel.conditions
        hoeck.library
        clojure.contrib.pprint
	clojure.contrib.except)
  (:require [clojure.set :as set]))

(use 'hoeck.rel.testdata)

(defmethod relation clojure.lang.IPersistentVector
  ;; (relation [:name :path] [:c :d :e :f]) -> #{{:name :c :path :f} ..}
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
  ([R conditions]
     (let [cmeta (map #(condition-meta %) conditions)
           prj-fields (map :name cmeta)]
       (set (map (fn [tuple] 
                   (zipmap (map keyword prj-fields)
                           (map #(% tuple) conditions)))
                 R)))))

(defmethod select :clojure
  ([R condition]     
     (set (filter condition R))))

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
  ([R S join-condition]
     (let [{:keys [field-a field-b join-function join-symbol type]} (join-condition)]
       (fjoin R (fn [r-tuple]
                  (map #(when (join-condition r-tuple %)
                          (merge r-tuple %))
                       S))))))

(comment (join people address (join-condition = :adress-id :id)))

(defmethod outer-join :clojure
  ([R S join-condition]
     (let [{:keys [field-a field-b join-function join-symbol type]} (join-condition)]
       (fjoin R (fn [r-tuple] 
                  (map #(if (join-condition r-tuple %)
                          (merge r-tuple %)
                          r-tuple)
                       S))))))

(defmethod union        :clojure [& rels] (apply set/union rels))
(defmethod difference   :clojure [& rels] (apply set/difference rels))
(defmethod intersection :clojure [& rels] (apply set/intersection rels))

;; helpers for aggregate
(defn get-aggregate-conditions
  "Given some conditions, return all aggregate-conditions."
  [conditions]
  (filter #(= (condition-meta % :type) :aggregate) conditions))

(defn get-group-conditions
  "Given some conditions, the names of all identity-conditions
  (aka the groups in an aggregate operation)"
  [conditions]
  (->>  conditions
        (filter #(= (condition-meta % :type) :identity))
        (map #(condition-meta % :name))))

(defmethod aggregate :clojure
  [R conditions] ;; identity-conditions and or aggregate-condition
  (let [aggregates (get-aggregate-conditions conditions)
        groups (get-group-conditions conditions)
        agg-names (map #(condition-meta % :name) aggregates)
        index (set/index R groups)]
    (set (map (fn [[k v]]
                (merge k (zipmap agg-names
                                 (map #(% v) aggregates))))
              index))))

(comment (aggregate agg-test-rel [(identity-condition :group)
                                  (aggregate-condition :count :age)]))

