
(ns hoeck.rel.sql.jdbc
  (:use hoeck.rel        
        [clojure.contrib.sql.internal :only [*db*]])
  (:require [hoeck.rel.operators :as rel-op])
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
  (.getMetaData (:connection *db*)))

(defn metadata-relations
  ([] (metadata-relations (:connection *db*)))
  ([connection]
     (.getMetaData connection)))


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
  in the (tables) relation"
  [table-name]
  (:table_name (first (select (tables) (= (.toLowerCase ~table_name) 
                                          (.toLowerCase (str table-name)))))))

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

(defn table-fields [table-name]
  (let [c (columns table-name)] ;; <--- continue HERE!
    (project c
             [(-> ~column_name .toLowerCase symbol) :field-name]
             [~ordinal_position :position]
             [(= ~is_autoincrement "YES") :autoincrement]
             )))

(defn primary-key-columns
  "return a set of columnnames which form the primary key of the given table-name."
  [table-name]
  (set (map #(-> % :column_name .toLowerCase symbol)
            (primary-keys (find-table table-name)))))

(comment ;; examples
  (rpprint (primary-keys 'person))
  (rpprint (select (tables) (= ~table_type "TABLE")))
  (rpprint (columns))
)

