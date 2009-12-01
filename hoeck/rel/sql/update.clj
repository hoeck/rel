
(ns hoeck.rel.sql.update
  (:use hoeck.rel.sql
        hoeck.rel.conditions ;; for update-where        
        clojure.contrib.pprint)
  (:require [hoeck.rel.sql.jdbc :as jdbc]
	    [clojure.contrib.sql :as csql]))


;; sql data manipulation

;; updating with expressions, like update sometable set name = 'prefix_' + name where status = 10

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
(comment ;; <--- continue HERE
  (defn update-relation 
    "Given an sql-relation R
  Useful for small relations R, cause the whole R must be traversed in order to
  change values."
    ([R] (let [conn (:connection *db*)]
           (when (nil? conn)
             (throwf "connection is nil, use set-connection to establish one"))
           (update-relation R conn)))
    [R conn]
    (let [expr (.sql R)]    
      (with-open [s (.prepareStatement conn expr)]
        (let [rs (.executeQuery s)
              rsmeta (.getMetaData rs)
              idxs (range 1 (inc (. rsmeta (getColumnCount))))
            
              ]
          (while (.next rs) 
            (when ())
            )
          )))))


;; todo: tracking changes to relations made with (conj R {:new 'tuple})
;;       in the relations metadata, then extract and upate


;; updating a relation by using delete and insert
	 

(defn delete-insert [table changeset]
  

  )


