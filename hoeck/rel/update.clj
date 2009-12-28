
(ns hoeck.rel.update
  ;;(:require [hoeck.rel :as rel])
  )

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
    (if (not= t new-tuple)
      (conj (disj R tuple) (vary-meta (merge t new-tuple)
                                      assoc :updated m))
      R)))

(def new-id-seq (map #(symbol (str "new-id-" %)) (range 0 1999)))

(defn insert
  "Insert a tuple into a relation. Collect change history in metadata.
  If R has an autoincremented primary key field, generate a temporary
  unique id for this one field."
  ([R new-tuple]
     (let [m (:inserts (meta R) [])
           auto-pkey (->> (-> R meta :fields)
                          (filter #(and (-> % meta :primary-key)
                                        (-> % meta :autoincrement)))
                          first keyword)
           new-ids (or (:new-ids (meta R)) new-id-seq)
           new-tuple (if auto-pkey
                       (assoc new-tuple auto-pkey (first new-id-seq))
                       new-tuple)
	   ;; mark the new tuple, so it is only inserted, not updated
	   new-tuple (vary-meta new-tuple assoc :inserted true)]
       (vary-meta (conj R new-tuple) assoc
                  :inserts (conj m new-tuple)
                  :new-ids (if auto-pkey
                             (rest new-ids)
                             nil))))
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
       (remove #(-> % meta :inserted)
	       (filter #(-> % meta :updated) R))))

(defn deletes
  "return a seq of deleted tuples from relation R."
  [R]
  (-> R meta :deletes))

(defn inserts
  "Return a seq of inserted tuples from relation R."
  [R]
  (-> R meta :inserts))
