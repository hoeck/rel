
(ns hoeck.mapped-map
  (:import
   (hoeck.mapped_map MappedMap)))

;; goal: Allow the implementation of lazy, combinable indexes

(defn mapped-map
  "Creates a new mapped map, like calling (into {} (map f hashmap)) but lazy.
  With [f m]: maps all values of the map using function into a new map.
  With [key-lookup-fn, key-f, val-f, m] maps MapEntrys of m using key-f and val-f where 
    key-lookup-fn is used to map new-keys to old-keys (for dissoc,
    assoc and friends of the mapped map)."
  ([] (MappedMap. identity identity identity {}))
  ([f m] ;; map only vals, leave keys alone
     (MappedMap. identity identity f m))
  ([key-lookup-fn, key-map-fn, val-map-fn, m]
     ;; map key & vals, provide a reverse-lookup function for keys
     (MappedMap. key-lookup-fn, key-map-fn, val-map-fn, m)))
