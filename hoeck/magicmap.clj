
(ns hoeck.magicmap
  (:import
   (hoeck.magicmap MagicMap)))

;; goal: Allow the implementation of lazy, combinable indexes

(defn magicmap
  "Creates a new magicmap."
  ([] (magicmap {}))
  ([hashmap-or-fn]
     (let [m (if (map? hashmap-or-fn) hashmap-or-fn {})
           f (if (map? hashmap-or-fn) (fn ([] ()) ([_])) hashmap-or-fn)]
       (hoeck.magicmap.MagicMap. {:map m :set #{} :fn f})))
  ([hashmap function]
     (hoeck.magicmap.MagicMap. {:map hashmap :set #{} :fn function})))


