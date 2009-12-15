
(ns hoeck.rel.update)

;; keeping a chance history in metadata:

;;   | location | key      | description
;; 0 | tuple    | :updated | the original tuple
;; 1 | set      | :deletes | seq of tuples to be deleted from the set
;; 2 | set      | :inserts | seq of new tuples

(defn update
  "Update a relation tuple. Collect change history in metadata.
  Each updated tuple has an :original original-tuple
  metadata-entry."
  [R tuple & key-value-pairs-or-hashmap]
  (let [a key-value-pairs-or-hashmap
        new-tuple (if (map? (first a)) (first a) (apply hash-map a))
        t (R tuple)
        m (:updated (meta t) t)]
    (conj (disj R tuple) (vary-meta (merge t new-tuple)
                                    assoc :updated m))))

(defn insert
  "Insert a tuple into a relation. Collect change history in metadata."
  ([R new-tuple]
     (let [m (:inserts (meta R) [])]
       (vary-meta (conj R new-tuple) assoc :inserts (conj m new-tuple))))
  ([R new-tuple & more-new-tuples]
     (reduce insert (insert R new-tuple) more-new-tuples)))

(defn delete
  "remove tuple from the relation. Collect change history in metadata."
  ([R tuple]
     (let [m (:deletes (meta R) [])
           t (disj R tuple)]
       (vary-meta (disj R tuple)
                  assoc :deletes (conj m tuple))))
  ([R tuple & tuples]
     (reduce remove (remove R tuple) tuples)))


;; change history accessors

(defn updates
  "Return a seq of [original-tuple altered-tuple] for all updated
  tuples of relation R."
  [R]
  (map #(vector (-> % meta :updated) %)
       (filter #(-> % meta :updated) R)))

(defn deletes
  "return a seq of deleted tuples from relation R."
  [R]
  (-> R meta :deletes))

(defn inserts
  "Return a seq of inserted tuples from relation R."
  [R]
  (-> R meta :inserts))
