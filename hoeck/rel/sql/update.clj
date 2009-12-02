
(ns hoeck.rel.sql.update
  (:use hoeck.rel
	hoeck.rel.sql
        hoeck.rel.conditions ;; for update-where        
        clojure.contrib.pprint
	clojure.contrib.except)
  (:require [hoeck.rel.sql.jdbc :as jdbc]
	    [clojure.contrib.sql :as csql]
	    [clojure.contrib.sql.internal :as csqli])
  (:import (java.sql ResultSet
		     Statement
		     SQLFeatureNotSupportedException)))


;; sql data manipulation

(defn get-connection []
  (or (:connection csqli/*db*)
      (throwf "connection is nil, use set-connection to establish one")))

;; updating with expressions, like: "update sometable set name = 'prefix_' + name where status = 10"
(defn update-where* [table-name where-condition set-conditions]
  (let [expr (cl-format nil "update ~a set ~:{~a=~a~:^, ~} where ~a"
                        (sql-symbol (name table-name))
                        (map #(vector (sql-symbol (name (condition-meta % :name)))
                                      (condition-sql-expr %))
                             set-conditions)
                        (condition-sql-expr where-condition))]
    ;;(sql-execute )
    expr))

(defmacro update-where
  "an sql \"update set name-condition-pairs where where-condition\" like statement, example:
  (update-where 'personen (< ~id 1000) :id (+ ~id 1) :name (str ~name \"_x\"))"
  [table-name where-condition & field-condition-pairs]
  `(update-where* ~table-name (condition ~where-condition)
                  ~(vec (map (fn [[field cond-expr]]
                                `(condition ~cond-expr ~field))
                              (partition 2 field-condition-pairs)))))


;; updating a table using a changeset

(defn update-table
  "Update the table table-name with the tuples in R where
  the primary key of table-name is matched against the corresponding
  field(s) in R.
  Useful for small changesets and big tables."
  [table-name tuple-seq]
  (let [update-stmt (fn [name-value-pairs, pkey-value-pairs] 
                      (cl-format nil "update ~a set ~:{~a=~a~:^, ~} where ~:{~a=~a~:^ and ~}"
                                 (sql-symbol table-name)
                                 name-value-pairs
                                 pkey-value-pairs))
        sql-pairs (fn [[k v]] [(sql-symbol k) (sql-print v)])
        pkey (map keyword (jdbc/primary-key-columns table-name))]
    (csql/transaction
     (doseq [t tuple-seq]
       ;;sql-execute 
       (let [vals (map sql-pairs (apply dissoc t pkey))]
	 (when (not (empty? vals))
	   (sql-execute (update-stmt vals (map sql-pairs (select-keys t pkey))))))))))

(comment (def rr (relation 'person 'id 'status 'name))
         (update-table 'person
                       ;; changeset, applied using update-clause
                       '({:id 1000 :name "frank"}
                         {:id 1000 :status -1}
                         {:id 1001 :name "erhardt" :status 0}))
)


;; updating a relation using its resultset and another relation
;; versatile but limited for big Rs

(defn- update-resultset
  "function to do transaction handling, querying and closing the resultset as
  well as providing additional information for using a resultsets .updateXXX 
  methods/capabilities."
  ([R func] (csql/transaction (update-resultset R (get-connection) func)))
  ([R conn func]
     (let [expr (.sql_expr R)]
       (with-open [s (.prepareStatement conn
					expr
					ResultSet/TYPE_FORWARD_ONLY
					ResultSet/CONCUR_UPDATABLE)]
	 (let [rs (.executeQuery s)
	       rsmeta (.getMetaData rs)
	       idxs (range 1 (inc (. rsmeta (getColumnCount))))
	       name-idx-map (zipmap (map #(-> (.getColumnName rsmeta %) .toLowerCase keyword) idxs)
				    idxs)
	       pk (set (map #(-> % name keyword) (filter #(:primary-key (meta %)) (fields R))))
	       pk-idxs (map name-idx-map pk)
	       get-pk (fn [] (zipmap pk (map #(.getObject rs %) pk-idxs)))]
	   (func rs ;; the resultset, step with (while (.next rs) ...)
		 rsmeta ;; java.sql.ResultSetMetaData
		 name-idx-map ;; map of :column-name to rs column-index number
		 get-pk ;; return a primary-key map from a resultset-row
		 pk ;; a set of :primary-key-fields
		 ))))))

(defn update-relation 
  "Given an sql-relation R, apply all changes made to R (changes are updates to non-primary
  key fields, stored as metadata in the relations tuples.
  Useful for small relations R, cause the whole resultset must be traversed in order to
  change values."
  ([R]
     (update-resultset 
      R
      (fn [rs rsmeta name-idx-map get-pk pk]
	(while (.next rs)
	  (let [updates (-> (get-pk) R meta :update)]
	    (doseq [u updates]
	      (doseq [[name value] u]
		(when-not (pk name) ;; don't update primary keys
		  (when (.isWritable rsmeta (name-idx-map name))
		    ;; only write into writeable columns
		    (.updateObject rs (name-idx-map name) value))))
	      (.updateRow rs))))))))

(defn insert-relation
  "Insert all new tuples of R into its originating table(s)."
  [R]
  (update-resultset
   R
   (fn [rs rsmeta name-idx-map get-pk pk]
     (.moveToInsertRow rs)
     (doseq [i (-> R meta :inserts)]
       (doseq [[name value] (get R i)]
	 (let [idx (name-idx-map name)]
	   (when (and (not (.isAutoIncrement rsmeta idx)) (.isWritable rsmeta idx))
	     ;; ignore autoincremented pk-values and non-writable columns
	     (.updateObject rs idx val))))
       ;; javadoc: All of the columns in a result set must be given a value each time this method is called before calling insertRow
       (.insertRow rs)))))


;; using prepared-statements

(defn insert-table
  "Insert values of tuples into table.
  Columns must match be keys in the tuple-hashmaps."
  ([table-name columns tuples]
     (insert-table (get-connection) table-name columns tuples))
  ([conn table-name columns tuples]
     (if (< 0 (count tuples))
       (let [fs (seq columns)
	     expr (cl-format nil "insert into ~s (~{~a~^, ~}) values (~{~a~^, ~})"
			     table-name (map sql-symbol fs) (take (count fs) (repeat "?")))
	     prep (.prepareStatement conn expr)] ;;Statement/RETURN_GENERATED_KEYS
	 (doseq [t tuples]
	   (doseq [col fs
		   idx (range 1 (inc (count fs)))]
	     (.setObject prep idx (get t col)))
	   (.addBatch prep))
	 (seq (.executeBatch prep))
	 ;; (try (when (.supportsGetGeneratedKeys (.getMetaData conn))
	 ;;      (relation (.getGeneratedKeys prep)))
	 ;;   (catch Exception e nil))
	 ))))


