
(ns hoeck.value-mapped-map.ValueMappedMap
  (:require [hoeck.library :as library]
            [de.kotka.lazymap :as lazymap])
  (:gen-class
   :init         init
   :state        state
   :implements   [clojure.lang.IPersistentMap clojure.lang.IFn
                  clojure.lang.IObj]
   :methods      [[lazyAssoc [Object Object] clojure.lang.IPersistentMap]]
   :constructors {[clojure.lang.IFn clojure.lang.IPersistentMap] []
                  [clojure.lang.IFn clojure.lang.IPersistentMap clojure.lang.IPersistentMap] []})
  (:import
     (clojure.lang SeqIterator IMapEntry IPersistentVector IObj MapEntry)
     (de.kotka.lazymap LazyMapEntry)))

(library/eval-when (library/class-exists? 'hoeck.value_mapped_map.ValueMappedMap)

;; constructor
(defn- -init
  ([f, m]
     [[] [f m nil]])
  ([f, m, n]
     [[] [f m n]]))

;; self
(defmacro state
  ([] `(.state ~'this))
  ([thing]
     `((state) ~({:fn 0 :map 1 :new-map 2} thing))))

(defmacro mutate-self
  "Construct a new Object using its current state and the
  given state-key/value pairs."
  [& statekey-newvalue]
  `(hoeck.value_mapped_map.ValueMappedMap. 
    ~@(let [new-state (apply hash-map statekey-newvalue)
            ctor-args '(:fn :map :new-map)]
        (map (fn [key] 
               (if (contains? new-state key)
                 (new-state key)
                 `(state ~key))) ctor-args))))


; IObj
(defn- -meta
  [this]
  (meta (state :map)))

(defn- -withMeta
  [this meta-data]
  (mutate-self :map (with-meta (state :map) meta-data)))

; IPersistentMap
(defn- -assoc
  [this k v]
  (mutate-self :new-map (assoc (or (state :new-map) (empty (state :map))) k v)
               :map (dissoc (state :map) k)))

(defn- -assocEx
  [this k v]
  (when (. this containsKey k)
    (throw (new Exception (str "Key alread present: " k))))
  (. this assoc k v))

(defn- -without
  [this k]
  (mutate-self :map (dissoc (state :map) k)
               :new-map (if (state :new-map) (dissoc (state :new-map) k))))

; LazyMap (in case (state :map) is a lazymap)
(defn -lazyAssoc [this k v]
  (mutate-self :new-map (lazymap/lazy-assoc* (or (state :new-map) (empty (state :map))) k v)
               :map (dissoc (state :map) k)))

; Associative
(defn- -containsKey
  [this k]
  (or (contains? (state :new-map)) 
      (contains? (state :map))))

(defn- -entryAt
  [this k]
  (or (and (state :new-map)
           (find (state :new-map) k))
      (and (contains? (state :map) k)
           (LazyMapEntry. k (delay ((state :fn) ((state :map) k)))))))

(defn- -valAt
  ([this k]
   (. this valAt k nil))
  ([this k nf]
     (or (and (contains? (state :new-map) k)
              ((state :new-map) k))
         (and (contains? (state :map) k)
              ((state :fn) ((state :map) k))))))

; Iterable
(defn- -iterator
  [this]
  (new SeqIterator (. this seq)))

; IPersistentCollection
(defn- -count
  [this]
  (count (set (concat (keys (state :map)) (keys (state :new-map))))))

(defn- -seq
  [this]  
  (let [n (state :new-map)
        m (state :map)
        f (state :fn)]
    (concat n (map (fn [[k v]]
                     (if (not (contains? n k))
                       (LazyMapEntry. k (delay (f v)))))
                   m))))

(defn- -cons
  [this o]
  (mutate-self :new-map (conj (state :new-map) o)))

(defn- -empty
  [this]
  (empty (state :map))
  ;(mutate-self :fn identity :map  :new-map nil)
  )

; IFn
(defn- -invoke
  ([this k]
     (. this valAt k))
  ([this k d]
     (. this valAt k d)))

(defn- -applyTo
  [this ks]
  (condp = (count ks)
    1 (.valAt this (first ks))
    2 (.valAt this (first ks) (second ks))
    (throw (IllegalArgumentException. "Wrong number of args passed to: ValueMappedMap"))))


)

;; +++ bootstrap +++
;; (require 'hoeck.value-mapped-map.ValueMappedMap :reload)
;; (binding [*compile-path* "/home/timmy-turner/clojure/classes"] (compile 'hoeck.value-mapped-map.ValueMappedMap))


