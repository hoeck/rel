
(ns hoeck.mapped-map.MappedMap
  (:require [hoeck.library :as library])
  (:gen-class
   :init         init
   :state        state
   :methods      [[lazyAssoc [Object Object] clojure.lang.IPersistentMap]]
   :implements   [clojure.lang.IPersistentMap clojure.lang.IFn
                  clojure.lang.IObj]
   :constructors {[clojure.lang.IFn clojure.lang.IFn clojure.lang.IFn clojure.lang.IPersistentMap] []
                  [clojure.lang.IPersistentMap] []})
  (:import
     (clojure.lang SeqIterator IMapEntry IPersistentVector IObj MapEntry)))

(library/eval-when (library/class-exists? 'hoeck.mapped_map.MappedMap)

(defn- -init
  ([key-look-fn, key-fn, val-fn, m]
  ;; key-look-fn: maps new-keys -> old-key
  ;; key-fn maps keys
  ;; val-fn dito
  ;; m: tha hash-map to map
     [[] {:look key-look-fn, :key key-fn, :val val-fn, :map m}])
  ([state-map]
     [[] state-map]))

(defmacro state
  ([] `(.state ~'this))
  ([thing] `((state) ~thing)))

; IObj
(defn- -meta
  [this]
  (.meta (state :map)))

(defn- -withMeta
  [this meta-data]
  (hoeck.mapped_map.MappedMap. (.assoc (state) 
                            :map
                            (.withMeta  (state :map) meta-data))))

; IPersistentMap
(defn- -assoc ;; calculates the whole map
  [this k v]
  (with-meta (into {} (concat (.seq this) [k v])))
             (.meta this))

(defn- -assocEx
  [this k v]
  (when (. this containsKey k)
    (throw (new Exception (str "Key alread present: " k))))
  (. this assoc k v))

(defn- -without
  [this k]
  (hoeck.mapped_map.MappedMap. (assoc (state) :map 
                           (.without (state map) ((state :look) k)))))

; Associative
(defn- -containsKey
  [this k]
  (.contains (state :map) ((state :look) k)))

(defn- -entryAt
  [this k]
  (clojure.lang.MapEntry. k (.valAt (state :map) ((state :look) k))))

(defn- -valAt
  ([this k]
   (. this valAt k nil))
  ([this k nf]
   (if (.containsKey (state :map) k)
     ((state :val) ((state :map) ((state :look) k)))
     nf)))

; Iterable
(defn- -iterator
  [this]
  (new SeqIterator (. this seq)))

; IPersistentCollection
(defn- -count
  [this]
  (.count (state :map)))

(defn- -seq
  [this]
  (let [keyfn (state :key), valfn (state :val)]
    (map (fn [[k v]] (MapEntry. (keyfn k) (valfn v))) (state :map))))

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
  (hoeck.mapped_map.MappedMap. (assoc (state) :map 
                           {})))

; IFn
(defn- -invoke
  [this & ks]
  (. this valAt (first ks)))

(defn- -applyTo
  [this ks]
  (. this valAt (first ks)))


)

;; (require 'hoeck.mapped-map.MappedMap :reload)
;; (binding [*compile-path* "/home/timmy-turner/clojure/classes"] (compile 'hoeck.mapped-map.MappedMap))
