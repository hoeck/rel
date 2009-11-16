;   Copyright (c) 2008, Erik Soehnel All rights reserved.
;
;   Redistribution and use in source and binary forms, with or without
;   modification, are permitted provided that the following conditions
;   are met:
;
;     * Redistributions of source code must retain the above copyright
;       notice, this list of conditions and the following disclaimer.
;
;     * Redistributions in binary form must reproduce the above
;       copyright notice, this list of conditions and the following
;       disclaimer in the documentation and/or other materials
;       provided with the distribution.
;
;   THIS SOFTWARE IS PROVIDED BY THE AUTHOR 'AS IS' AND ANY EXPRESSED
;   OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
;   WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;   ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY
;   DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
;   DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
;   GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;   INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;   WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;   NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;   SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

(ns hoeck.rel.sql-utils
  (:require [clojure.contrib.sql.internal :as con-sql-int]
            hoeck.rel)
  (:use hoeck.library, clojure.contrib.fcase)
  (:import (java.sql Connection Types)))

(def default-derby-args
     {:classname "org.apache.derby.jdbc.EmbeddedDriver"
      :subprotocol "derby"
      :subname "g:/clojure/test.db"
      ;;:ta-level :repeatable-read
      ;;:autocommit false
      'create false})       ;; symbols: connection properties

(def default-sybase-args
     {:classname "com.sybase.jdbc2.jdbc.SybDriver" 
      :subprotocol "sybase:Tds" ;; ^= what
      :subname "localhost:2638/temp" ;; ^= where
      ;; symbols: connection properties
      'user "dbo"
      'password ""
      'create false})

(def transaction-levels
  {:none Connection/TRANSACTION_NONE
   :read-commited Connection/TRANSACTION_READ_COMMITTED
   :read-uncommited Connection/TRANSACTION_READ_UNCOMMITTED
   :repeatable-read Connection/TRANSACTION_REPEATABLE_READ
   :serializable Connection/TRANSACTION_SERIALIZABLE})

(defn supported-transaction-levels [conn]
  (let [m (.getMetaData conn)]
    (if (.supportsTransactions m)
      (map first (filter #(.supportsTransactionIsolationLevel m (val %)) transaction-levels)))))
      
(defn get-connection
  "Opens a new connection to a database. db-spec is a map containing string
  values for these required keys:
    :classname     the jdbc driver class name
    :subprotocol   the jdbc subprotocol
    :subname       the jdbc subname
  db-spec may contain additional key-value pairs that are passed along to
  the driver as properties such as 'user, 'password, etc."
  [& db-spec-keyargs]
  (let [db-spec (apply hash-map db-spec-keyargs)
        _ (Class/forName (:classname db-spec))
        conn (java.sql.DriverManager/getConnection
              (format "jdbc:%s:%s" (:subprotocol db-spec) (:subname db-spec))
              (con-sql-int/properties (reduce (fn [r k] (if (keyword? k) (dissoc r k) r))
					      db-spec (keys db-spec))))]
    (when-let [ta (db-spec :ta-level)] (.setTransactionIsolation conn (transaction-levels ta)))
    (when-let [ac (db-spec :autocommit)] (.setAutoCommit conn ac))
    conn))

(defn resultvec
  "Make a vector of tuples from an sql resultset."
    [rs]
    (let [idxs (range 1 (inc (.getColumnCount (.getMetaData rs))))
          row-values (fn [] (vec (map (fn [#^Integer i] (. rs (getObject i))) idxs)))]
      (loop [rows []]
        (if (.next rs)
          (recur (conj rows (row-values)))
          rows))))

(defn sql-query-rs
  "Prepare and execute query in sql and return a java.sql.ResultSet."
  [conn sql]
  (.executeQuery (.prepareStatement conn sql)))

(defn sql-query
  "Prepare and execute query in sql, .close query and return a vector."
  [conn sql]
  (with-open [s (.prepareStatement conn sql)]
      (resultvec (.executeQuery s))))

(defn sql-command
  "Execute sql commands."
  [conn & sql]
  (with-open [stmt (.createStatement conn)]
      (doseq [cmd sql]
          (.addBatch stmt cmd))
    (.executeBatch stmt)))

;; tools
(defn estr
  "remove or excape all `'' from string (str s)"
  [s]
  (.remove (str s) \'))



(defn name->sql
  "Converts a clojure symbol to a sql-identifier."
  [clj-name]
  (valid-name! (.replace (if (string? clj-name) clj-name (name clj-name)) \- \_)))

(defn name->clj
  "Converts a sql-identifier-string into a lowercase clojure symbol."
  [sql-name]
  (symbol (.toLowerCase (.replace sql-name \_ \-))))

(defn add-commata
  "(add-commata '(\"a\" \"b\" \"c\")) -> \"a, b, c\""
  [str-seq]
  (reduce #(str % ", " %2) str-seq))

(def types (hoeck.rel/make-relation
  '[type sql java-num default-precision]
  :keyword "varchar" Types/VARCHAR 30
  :symbol "varchar" Types/VARCHAR 30
  :string "varchar" Types/VARCHAR 200
  :int "integer", Types/INTEGER nil
  :date "date", Types/DATE nil
  :time "time", Types/TIME nil
  :timestamp "timestamp" Types/TIMESTAMP nil))

;; maps keywords to sql
(def type-string (hoeck.rel/group-by (hoeck.rel/project types *type *sql) 'type))

;; maps Types to keywords
(def type-num (hoeck.rel/group-by (hoeck.rel/select (hoeck.rel/project types *type *java-num) (and (not= *type :keyword) (not= *type :symbol))) 'java-num))

(defn probe-resultset
  "Retrieve relation metadata (eg columnnames) from an sql Relation."
  [resultset]
  (let [rs-meta (.getMetaData resultset)
        col-index (range 1 (inc (.getColumnCount rs-meta)))
        names (vec (map #(name->clj (.getColumnName rs-meta %)) col-index))
        types (vec (map #(type-num (.getColumnType rs-meta %)) col-index))
        precs (vec (map #(.getPrecision rs-meta %) col-index))]
    {:fields names
     :types types
     :precs precs}))

(defn probe-resultset-r
  [rs]
  (let [{:keys [fields, types, precs]} (probe-resultset rs)]
    (apply hoeck.rel/make-relation '[pos #^{:primary-key true} field type prec]
           (mapcat list (iterate inc 0) fields types precs))))

(defn make-connection-fn
   "Return a function r with the following spec:
  (r select-stmt) : return a seq of vectors, may be lazy
  (r :rs select-stmt) : return the resultset for the select-statement
  (r :command sql-stmt) : execute an sql command (alter table etc.)
  [(r :conn: _) : optionally return a java.sql.Connection object or nil.]"
   [& db-spec]
   (let [conn (delay (get-connection (as-keyargs db-spec)))]
     (fn ([stmt] (sql-query (force conn) stmt))
         ([key stmt] (condp = key
                       :rs (sql-query-rs (force conn) stmt)
                       :command (let [r (seq (sql-command (force conn) stmt))] (and (seq? r) (if (rest r) r (first r))))
                       :conn (force conn))))))

