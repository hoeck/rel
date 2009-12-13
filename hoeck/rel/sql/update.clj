
(ns hoeck.rel.sql.update
  (:use hoeck.rel	
        hoeck.rel.conditions ;; for update-where        
        clojure.contrib.pprint
	clojure.contrib.except)
  (:require [hoeck.rel.sql.jdbc :as jdbc]
            [hoeck.rel.sql :as sql]
            [clojure.set :as set])
  (:import (java.sql ResultSet
		     Statement
		     SQLFeatureNotSupportedException)))


;; updating with expressions, like:
;;   "update sometable set name = 'prefix_' + name where status = 10"
;;   (update-where sometable (= ~status 10) :name (str "prefix_" ~name))

(defn update-where* [table-name where-condition set-conditions]
  (let [expr (cl-format nil "update ~a set ~:{~a=~a~:^, ~} where ~a"
                        (sql-symbol (name table-name))
                        (map #(vector (sql-symbol (name (condition-meta % :name)))
                                      (condition-sql-expr %))
                             set-conditions)
                        (condition-sql-expr where-condition))]
    (sql/execute expr)))

(defmacro update-where
  "an sql \"update set name-condition-pairs where where-condition\" like statement, example:
  (update-where 'personen (< ~id 1000) :id (+ ~id 1) :name (str ~name \"_x\"))"
  [table-name where-condition & field-condition-pairs]
  `(update-where* ~table-name (condition ~where-condition)
                  ~(vec (map (fn [[field cond-expr]]
                                `(condition ~cond-expr ~field))
                              (partition 2 field-condition-pairs)))))


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
        new-tuple (if (map? (first a)) a (apply hash-map a))
        t (R tuple)
        m (:updated (meta t) t)]
    (conj (disj R tuple) (vary-meta (merge t new-tuple)
                                    assoc :updated m))))

(defn insert
  "Insert a tuple into a relation. Collect change history in metadata."
  [R new-tuple]
  (let [m (:inserts (meta R) [])]
    (vary-meta (conj R new-tuple) assoc :inserts (conj m new-tuple))))

(defn delete
  "remove tuple from the relation. Collect change history in metadata."
  [R tuple]
  (let [m (:deletes (meta R) [])
        t (disj R tuple)]
    (vary-meta (disj R tuple)
               assoc :deletes (conj m tuple))))


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


;; relation field metadata

(defn primary-key? [field] (-> field meta :primary-key))
(defn autoincrement? [field] (-> field meta :autoincrement))

(defn primary-key-fields [fields] (map keyword (filter primary-key? fields)))
(defn value-fields [fields] (map keyword (remove primary-key? fields)))
(defn autoincrement-fields [fields]
  (map keyword
       (filter #(and (primary-key? %)
                     (autoincrement? %))
               fields)))

(defn- sql-pair
  "Return a pair of [symbol value] from field and value."
  [f v]
  [(sql/sql-symbol f) (sql/sql-print v)])

;; updating

(defn table-update-expr
  "Given a table name, fields, identity fields and an old and the new tuple,
  return an SQL update statement only updating the changed values."
  ([name fields identity-fields old-tuple new-tuple]
     (let [changed-keys (map key (remove (set old-tuple) (select-keys new-tuple fields)))]
       (cl-format nil "update ~a set ~:{~a=~a~:^, ~} where ~:{~a=~a~:^ and ~}"
                  (sql/sql-symbol name)
                  (map #(sql-pair % (new-tuple %)) changed-keys)
                  (map #(sql-pair % (old-tuple %)) identity-fields)))))

(defn table-update
  "Given a table name, fields to update and a seq of [old-tuple, new-tuple] pairs,
  run update statements on the current connection so that the table finally reflects
  the values from all new-tuples."
  [name fields old-new-tuple-seq]
  (when-not (empty? old-new-tuple-seq)
    (let [identity-keys (primary-key-fields fields)
          fs-keys (map keyword fields)]
      (sql/transaction
       (->> old-new-tuple-seq
            (map (fn [[old new]] (table-update-expr name fs-keys identity-keys old new)))
            (apply sql/execute))))))

(defn relation-tables
  "Read the metadata of R and return map of {table-name field-set, ..}."
  [R]
  (reduce (fn [m f]
            (update-in m [(-> f meta :table)] conj f))
          {} (fields R)))

(defn relation-update
  "given a relation R, update all changed tuples using sql update statements."
  [R]
  (let [u (updates R)]    
    (doseq [[table fields] (relation-tables R)]
      (table-update table fields u))))


;; inserting

(defn table-insert-expr
  "Given a table name, fields, identity fields and an old and the new tuple,
  return an SQL update statement only updating the changed values."
  ([name fields tuple]
     (cl-format nil "insert into ~a (~{~a~^, ~}) values (~{~a~^, ~})"
                (sql/sql-symbol name)
                (map sql-symbol fields)
                (map #(sql-print (tuple %)) fields))))

(defn table-insert
  "Insert values at fields from tuples into table name. Ignore tuple values for
  :autoincrement -ed fields. Return a seq of values of _one_ autoincrement
  field or nil."
  [name fields tuples]
  (when-not (empty? tuples)
    (let [fs-keys (value-fields fields)
          identity-keys (primary-key-fields fields)
          autoinc-field (first (autoincrement-fields fields))]
      ;; (try (when (.supportsGetGeneratedKeys (.getMetaData conn))
      ;;      (relation (.getGeneratedKeys prep)))
      ;;   (catch Exception e nil))
      (sql/transaction
       (->> tuples
            (map #(table-insert-expr name fs-keys %))
            (apply sql/execute-insert autoinc-field))))))

(defn relation-insert
  "Write inserted tuples into the (connection) database. If R is a joined
  relation, specify which table you want to update. Returns (inserts R) with
  possibly autogenerated keys.
  Only one autogenerated key per relation is read back."
  [R & [table-name]]
  (let [tables (relation-tables R)
        name (or table-name (first (keys tables)))
        fields (tables name)
        autoinc-key (first (autoincrement-fields fields))
        i (inserts R)
        generated-keys (table-insert name fields i)]
    (doall (map #(assoc %1 autoinc-key %2) i generated-keys))))


;; deleting

(defn table-delete-expr
  "Return an sql delete from (table-)name, identity-fields and tuples that
  identify those rows to be deleted."
  [name identity-fields tuple]
  (cl-format nil "delete from ~a where ~:{~a=~a~^, ~}"
             (sql/sql-symbol name)
             (map #(sql-pair % (tuple %)) identity-fields)))

(defn table-delete
  "Delete tuples from table name identified through fields."
  [name fields tuples]
  (when-not (empty? tuples)
    (let [pk-keys (primary-key-fields fields)]
      (sql/transaction
       (->> tuples
            (map #(table-delete-expr name pk-keys %))
            (apply sql/execute))))))

(defn relation-delete
  "Delete all disjoined tuples from R from its database table.
  If R is a joined relation, specify a table from where to delete rows."
  [R & [table-name]]
  (let [tables (relation-tables R)
        name (or table-name (first (keys tables)))
        fields (tables name)        
        d (deletes R)]
    (table-delete name fields R)))




