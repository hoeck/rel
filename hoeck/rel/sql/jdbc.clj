
(ns hoeck.rel.sql.jdbc
  (:use hoeck.rel
        clojure.contrib.except)
  (:require [hoeck.rel.operators :as rel-op]
            [hoeck.rel.sql :as sql]
            [hoeck.rel.non-lazy :as n]
            [hoeck.rel.expressions :as e] )
  (:import (java.sql ResultSet)))

(defmethod rel-op/relation ResultSet [rs] ;; create a clojure relation from a ResultSet
  (let [rsmeta (.getMetaData rs)
        idxs (range 1 (inc (.getColumnCount rsmeta)))
        fields (map (fn [idx]
                      (-> rsmeta
                          (.getColumnLabel idx)
                          .toLowerCase
                          symbol))
                    idxs)
        R (set (resultset-seq rs))]
    (with-meta R {:fields fields})))

(defn get-metadata []
  (.getMetaData (sql/connection)))

;; metadata relations

(defn table-types []
  (-> (get-metadata) .getTableTypes relation))

(defn tables
  "Return a relation of tables"
  []
  (relation (.getTables (get-metadata)
                        ""
                        nil
                        nil
                        (into-array String (map :table_type (table-types))))))

(defn find-table
  "given a symbol, return a string of the table name as present
  in the (tables) relation. Throw an exception if the table doesn't exist."
  [table-name]
  (or (:table_name (first (select* (tables)
                                   #(= (.toLowerCase (:table_name %)) 
                                       (.toLowerCase (str table-name))))))
      (throwf "Unknown table: %s" table-name)))

(defn primary-keys
  "Return a relation of tables and columnames which are primary keys.
  When given a table-name, limit the relation to pkeys of this table only."
  ([] (primary-keys nil))
  ([table-name]
     (->> (if table-name [(find-table table-name)] (map :table_name (tables)))
          (map #(.getPrimaryKeys (get-metadata)
                                 nil ;; catalog
                                 nil ;; schema
                                 %))
          (map relation)
          (apply union))))

(defn columns 
  "Return a relation of columnames, their types and tables. Given an optional
  tablename, return only columns of this table."
  ([] (-> (get-metadata) (.getColumns nil nil "%" "%") relation))
  ([table-name] (-> (get-metadata) (.getColumns nil nil (find-table table-name) "%") relation)))


;; tools

(defn table-fields
  "given a table-name, return all fields of this table in a set including field-metadata:
  :position, :autoincrement and :primary-key."
  [table-name]
  (map :field
       (project* (outer-join (columns table-name)
                             (project* (primary-keys table-name)
                                       (e/expr* :column_name :name)
                                       (e/expr* (constantly true) :primary-key))
                             (n/join= :column_name :name))
                 (e/expr* (fn [{:keys [column_name ordinal_position is_autoincrement primary-key]}]
                            (with-meta (-> column_name .toLowerCase symbol)
                                       {:table table-name
                                        :position (int ordinal_position)
                                        :autoincrement (= is_autoincrement "YES")
                                        :primary-key primary-key}))
                          :field))))

(defn primary-key-columns
  "return a set of columnnames which form the primary key of the given table-name."
  [table-name]
  (set (map #(-> % :column_name .toLowerCase symbol)
	    (primary-keys (find-table table-name)))))

(comment ;; examples
  (rpprint (primary-keys 'table))
  (rpprint (select (tables) #(= (:table_type %) "TABLE")))
  (rpprint (columns))
)

