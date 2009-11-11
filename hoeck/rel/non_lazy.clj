
;; eager evaluating implementation of the relational operators

(ns hoeck.rel.non-lazy
  (:use hoeck.rel.operators
        hoeck.rel.conditions
        hoeck.library
        clojure.contrib.pprint)
  (:require [clojure.set :as set]))

(use 'hoeck.rel.testdata)

(defmethod fields :clojure [R]
  (keys (first R)))

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

(defmethod aggregate :clojure
  [R conditions] ;; identity-conditions and or aggregate-condition
  (let [aggregates (filter #(= (condition-meta % :type) :aggregate) conditions)
        groups (->>  conditions
                     (filter #(= (condition-meta % :type) :identity))
                     (map #(condition-meta % :name)))
        agg-names (map #(condition-meta % :name) aggregates)
        index (set/index R groups)]
    (set (map (fn [[k v]]
                (merge k (zipmap agg-names
                                 (map #(% v) aggregates))))
              index))))

(comment (aggregate agg-test-rel [(identity-condition :group)
                                  (aggregate-condition :count :age)]))

