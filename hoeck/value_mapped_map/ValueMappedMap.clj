
;; a IPersistentMap implementation which takes a function and a hashmap
;; and maps the function lazily over the entries of the hashmap forming
;; new values


(ns hoeck.value-mapped-map.ValueMappedMap
  (:require [hoeck.library :as library]
            [de.kotka.lazymap :as lazymap])
  (:gen-class
   :init         init
   :state        state
   :extends      clojure.lang.APersistentMap
   :methods      [[lazyAssoc [Object Object] clojure.lang.IPersistentMap]]
   :constructors {[clojure.lang.IFn clojure.lang.IPersistentMap] []
                  [clojure.lang.IFn clojure.lang.IPersistentMap clojure.lang.IPersistentMap] []})
  (:import
     (clojure.lang SeqIterator IMapEntry IPersistentVector IObj MapEntry)
     (de.kotka.lazymap LazyMapEntry)))

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
  (or (contains? (state :new-map) k) 
      (contains? (state :map) k)))

(defn- -entryAt
  [this k]
  (or (and (find (state :new-map) k))
      (and (contains? (state :map) k)
           (LazyMapEntry. k (delay ((state :fn) ((state :map) k)))))
      nil))

(defn- -valAt
  ([this k]
   (. this valAt k nil))
  ([this k nf]
     (or (and (contains? (state :new-map) k)
              ((state :new-map) k))
         (and (contains? (state :map) k)
              ((state :fn) ((state :map) k)))
         nil)))

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
    (seq (lazy-cat n (filter identity (map (fn [e] (if (and e (not (contains? n (key e)))) ;; check why the e can be nil here !!!
                                                     (LazyMapEntry. (key e) (delay (f (val e))))))
                                           m))))))

(defn- -cons
  [this o]
  (mutate-self :new-map (conj (state :new-map) o)))

(defn- -empty
  [this]
  (empty (state :map)))

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

;; +++ bootstrap +++
;; (require 'hoeck.value-mapped-map.ValueMappedMap :reload)
;; (binding [*compile-path* "/home/timmy-turner/clojure/classes"] (compile 'hoeck.value-mapped-map.ValueMappedMap))
;; (binding [*compile-path* "g:\\clojure\\classes"] (compile 'hoeck.value-mapped-map.ValueMappedMap))


