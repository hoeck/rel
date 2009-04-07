
(ns hoeck.value-mapped-map
  (:import
   (hoeck.value_mapped_map ValueMappedMap)))

;; goal: Allow the implementation of lazy, combinable indexes

(defn value-mapped-map
  "Creates a lazy hashmap using a source hashmap and a function to lazily
  calculate the new map's values."
  ([] (ValueMappedMap. identity {}))
  ([f m] ;; map only vals, leave keys alone
     (ValueMappedMap. f m)))





