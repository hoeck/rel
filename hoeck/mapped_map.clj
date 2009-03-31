
(ns hoeck.mapped-map
  (:import
   (hoeck.mapped_map MappedMap)))

;; goal: Allow the implementation of lazy, combinable indexes

(defn mapped-map
  "Creates a new mapped map, like calling (into {} (map f hashmap)) but lazy."
  ([] (MappedMap. identity identity identity {}))
  ([f m] ;; map only vals, leave keys alone
     (MappedMap. identity identity f m))
  ([key-lookup-fn, key-map-fn, val-map-fn, m]
     ;; map key & vals, provide a lookup function for keys
     (MappedMap. key-lookup-fn, key-map-fn, val-map-fn, m)))
