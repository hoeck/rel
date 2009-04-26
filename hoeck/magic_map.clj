
(ns hoeck.magic-map
  (:import (hoeck.magic_map MagicMap)))

;; goal: Allow the implementation of lazy, combinable indexes

(defn magic-map
  "Creates a new magicmap. A magicmap implements IPersistentMap and
consists of the keys the function f returns when called without
arguments and the values of each key when the key is applied to
f. Optionally a additional hashmap may be supplied.
For side-effecting functions, one should use memoize around to prevent
violating the map's interface. Associng and dissocing works by keeping a
internal hashmap of assoc'd values and a set of dissoc'd keys, which
take precedence over the function generated values.
The Resulting map makes no difference between a key pointing to nil and
a key not being present at all."
  ([] (magic-map {}))
  ([hashmap-or-fn]
     (let [m (if (map? hashmap-or-fn) hashmap-or-fn {})
           f (if (map? hashmap-or-fn) (fn ([] ()) ([_])) hashmap-or-fn)]
       (MagicMap. {:map m :set #{} :fn f})))
  ([hashmap function]
     (MagicMap. {:map hashmap :set #{} :fn function})))


