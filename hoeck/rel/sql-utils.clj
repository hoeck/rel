;;;; DELETE ME

;(ns hoeck.rel.sql-utils)

;   (:use hoeck.library, clojure.contrib.fcase)
;   (:import (java.sql Connection)))

; (def default-derby-args
;      {:classname "org.apache.derby.jdbc.EmbeddedDriver"
;       :subprotocol "derby"
;       :subname "g:/clojure/test.db"
;       ;;:ta-level :repeatable-read
;       ;;:autocommit false
;       'create false})       ;; symbols: connection properties

; (def default-sybase-args
;      {:classname "com.sybase.jdbc2.jdbc.SybDriver" 
;       :subprotocol "sybase:Tds" ;; ^= what
;       :subname "localhost:2638/temp" ;; ^= where
;       ;; symbols: connection properties
;       'user "dbo"
;       'password ""
;       'create false})

; (def transaction-levels
;   {:none Connection/TRANSACTION_NONE
;    :read-commited Connection/TRANSACTION_READ_COMMITTED
;    :read-uncommited Connection/TRANSACTION_READ_UNCOMMITTED
;    :repeatable-read Connection/TRANSACTION_REPEATABLE_READ
;    :serializable Connection/TRANSACTION_SERIALIZABLE})

; (defn supported-transaction-levels [conn]
;   (let [m (.getMetaData conn)]
;     (if (.supportsTransactions m)
;       (map first (filter #(.supportsTransactionIsolationLevel m (val %)) transaction-levels)))))
      
; (defn get-connection
;   "Opens a new connection to a database. db-spec is a map containing string
;   values for these required keys:
;     :classname     the jdbc driver class name
;     :subprotocol   the jdbc subprotocol
;     :subname       the jdbc subname
;   db-spec may contain additional key-value pairs that are passed along to
;   the driver as properties such as 'user, 'password, etc."
;   [& db-spec-keyargs]
;   ;;(println :args db-spec-keyargs)
;   (let [db-spec (as-keyargs db-spec-keyargs)
;         _ (Class/forName (:classname db-spec))
;         conn (java.sql.DriverManager/getConnection
;               (format "jdbc:%s:%s" (:subprotocol db-spec) (:subname db-spec))
;               (con-sql-int/properties (reduce (fn [r k] (if (keyword? k) (dissoc r k) r)) db-spec (keys db-spec))))]
;     (if (not (nil? (db-spec :ta-level))) (.setTransactionIsolation conn (transaction-levels (db-spec :ta-level))))
;     (if (not (nil? (db-spec :autocommit))) (.setAutoCommit conn (db-spec :autocommit)))
;     conn))

; (defn lazy-resultset
;   [#^java.sql.ResultSet rs]
;     (let [idxs (range 1 (inc (.getColumnCount (.getMetaData rs))))
;           row-values (fn [] (vec (map (fn [#^Integer i] (. rs (getObject i))) idxs)))
;           rows (fn thisfn [] 
;                  (when (.next rs) (lazy-cons (row-values) (thisfn))))]
;       (rows)))

; (defn resultvec
;   "Make a vector of tuples from an sql resultset."
;   [#^java.sql.ResultSet rs]
;     (let [idxs (range 1 (inc (.getColumnCount (.getMetaData rs))))
;           row-values (fn [] (vec (map (fn [#^Integer i] (. rs (getObject i))) idxs)))]
;       (loop [rows []]
;         (if (.next rs)
;           (recur (conj rows (row-values)))
;           rows))))

; (defn sql-query-rs
;   "Prepare and execute query in sql and return a java.sql.ResultSet."
;   [conn sql]
;   (.executeQuery (.prepareStatement conn sql)))

; (defn sql-query
;   "Prepare and execute query in sql, .close query and return a vector."
;   [conn sql]
;   (with-open [s (.prepareStatement conn sql)]
;       (resultvec (.executeQuery s))))

; (defn sql-command
;   "Execute sql commands."
;   [conn & sql]
;   (with-open [stmt (.createStatement conn)]
;       (doseq [cmd sql]
;           (.addBatch stmt cmd))
;     (.executeBatch stmt)))

; (defn make-connection-fn
;   "Return a function which executes a given string as a sql query on a connection.
;    given a query and an optional :rs keyword, returns a resultset."
;   [& db-spec]
;   (fn [] (force (delay (get-connection (as-keyargs db-spec))))))

