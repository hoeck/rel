
(ns hoeck.magic-map.MagicMap
  (:require [hoeck.library :as library])
  (:gen-class
     :init         init
     :state        prop-map
     :methods      [[lazyAssoc [Object Object] clojure.lang.IPersistentMap]]
     :implements   [clojure.lang.IPersistentMap clojure.lang.IFn
                    clojure.lang.IObj]
     :constructors {[clojure.lang.IPersistentMap] []})
  (:import
     (clojure.lang SeqIterator IMapEntry MapEntry IPersistentVector IObj)))

(library/eval-when (library/class-exists? 'hoeck.magic_map.MagicMap)

(defn- -init
  [prop-map]
  [[] prop-map])

(defmacro prop [] `(.prop-map ~'this ))
(defmacro prop-map  [] `((prop) :map))
(defmacro prop-fn   [] `((prop) :fn))
(defmacro prop-set  [] `((prop) :set))
(defmacro prop-meta [] `((prop) :meta))


; IObj
(defn- -meta
  [this]
  (prop-meta))

(defn- -withMeta
  [this meta-data]
  (hoeck.magic_map.MagicMap. (assoc (prop) :meta meta-data)))

; IPersistentMap
(defn- -assoc
  [this k v]
  (hoeck.magic_map.MagicMap.
   (assoc (prop)
     :map (assoc (prop-map) k v)
     :set (disj (prop-set) k))))

(defn- -assocEx
  [this k v]
  (when (. this containsKey k)
    (throw (new Exception (str "Key alread present: " k))))
  (. this assoc k v))

(defn- -without
  [this k]
  (hoeck.magic_map.MagicMap. 
   (if ((prop-map) k)
     (assoc (prop) :map (dissoc (prop-map) k))
     (assoc (prop) :set (conj (prop-set) k)))))

; Associative
(defn- -containsKey
  [this k]
  (or (.containsKey (prop-map) k)
      (if ((prop-set) k)
        false
        (boolean ((prop-fn) k)))))

(defn- -valAt
  ([this k]
     (or ((prop-map) k)
         (if (not ((prop-set) k))
           ((prop-fn) k))))
  ([this k nf]
     (or (.valAt this k) nf)))

(defn- -entryAt
  [this k]
  (if-let [v (.valAt this k)]
    (clojure.lang.MapEntry. k v)))

; Iterable
(defn- -iterator
  [this]
  (new SeqIterator (. this seq)))

; IPersistentCollection
(defn- -count 
  [this]
  (-> this .seq count)) ;; count is expensive

(defn- -seq
  [this]
  (concat (prop-map)
          (map #(MapEntry. % ((prop-fn) %))
               (filter #(not (or ((prop-set) %)
                                 (contains? (prop-map) %)))
                       ((prop-fn))))))

(defn- -cons
  [this o]
  (condp instance? o
    IMapEntry         (let [k (. o getKey)
                            v (. o getValue)]
                        (. this assoc k v))
    IPersistentVector (if (= (count o) 2)
                        (let [k (o 0)
                              v (o 1)]
                          (. this assoc k v))
                        (throw (new IllegalArgumentException
                                    "Vector arg to map conj must be a pair")))
    (reduce #(. %1 cons %2) this o)))

(defn- -empty
  [this]
  (hoeck.magic_map.MagicMap. (-> this .theMap .empty)))

; IFn
(defn- -invoke
  [this & ks]
  (. this valAt (first ks)))

(defn- -applyTo
  [this ks]
  (. this valAt (first ks)))

(defn- -lazyAssoc
  [this k v]
  (hoeck.magic_map.MagicMap.
   (assoc (prop)
     :map (.lazyAssoc (prop-map) k v)
     :set (disj (prop-set) k))))

)

;; (require 'hoeck.magic-map.MagicMap :reload)
;; (binding [*compile-path* "/home/timmy-turner/clojure/classes"] (compile 'hoeck.magic-map.MagicMap))
;; (binding [*compile-path* "g:\\clojure\\classes"] (compile 'hoeck.magic-map.MagicMap))

