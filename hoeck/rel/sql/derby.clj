
(ns hoeck.rel.sql.derby
  (:use hoeck.rel.sql.db
        hoeck.rel.sql
        hoeck.rel)
  (:import (java.sql ResultSet Blob Clob Date Timestamp Time))
  (:gen-class :name :hoeck.rel.sql.derby.Function
              :methods [;; table function                v--varargs allowed???
                        #^{:static true} [invoke [String & Object] java.sql.ResultSet]
                        ]))

;; (server-side)(table-)functions for derby ;; INCOMPLETE

(defn invoke [this function-name & args])

;; (set (vals '{
;;              BIGINT 	getLong
;;              BLOB 	getBlob
;;              CHAR 	getString
;;              CHAR_FOR_BIT_DATA 	getBytes
;;              CLOB 	getClob
;;              DATE 	getDate
;;              DECIMAL 	getBigDecimal
;;              DOUBLE 	getDouble
;;              DOUBLE_PRECISION 	getDouble
;;              FLOAT 	getDouble
;;              INTEGER 	getInt
;;              LONG_VARCHAR 	getString
;;              LONG_VARCHAR_FOR_BIT_DATA 	getBytes
;;              NUMERIC 	getBigDecimal
;;              REAL 	      getFloat
;;              SMALLINT     getShort
;;              TIME 	     getTime
;;              TIMESTAMP 	        getTimestamp
;;              VARCHAR 	        getString
;;              VARCHAR_FOR_BIT_DATA 	getBytes
;;              }))
;; #{

;; CREATE FUNCTION externalEmployees
;; ()
;; RETURNS TABLE
;; (
;;   employeeId    INT,
;;   lastName      VARCHAR( 50 ),
;;   firstName     VARCHAR( 50 ),
;;   birthday      DATE
;; )
;; LANGUAGE JAVA
;; PARAMETER STYLE DERBY_JDBC_RESULT_SET
;; READS SQL DATA
;; EXTERNAL NAME 'com.acme.hrSchema.EmployeeTable.read'


;; {:employee-id :int
;;  :lastname    [:varchar 50]
;;  :firstname   [:varchar 50]
;;  :birthday    :data}


(defmacro #^{:private true} relation-field-get []
  `(get (first (deref ~'relation-seq-atom)) (~'field-idx-map ~'i)))

(deftype relation-resultset
  [;; an atom-wrapped seq
   relation-seq-atom
   ;; maps index number to field-name (for tuple lookup)
   field-idx-map]
  [ResultSet]
  (.next [] (boolean (seq (swap! relation-seq-atom pop))))
  (.close [])
  (.wasNull [] false)
  (#^float .getFloat [#^int i] (float (relation-field-get)))
  (#^BigDecimal .getBigDecimal [#^int i] (relation-field-get))
  (#^Date .getDate [#^int i] (relation-field-get))
  (#^Clob .getClob [#^int i] (relation-field-get))
  (#^long .getLong [#^int i] (long (relation-field-get)))
  (#^Time .getTime [#^int i] (relation-field-get))
  (#^short .getShort [#^int i] (short (relation-field-get)))
  (#^double .getDouble [#^int i] (double (relation-field-get)))
  (#^Blob .getBlob [#^int i] (relation-field-get))
  (#^Timestamp .getTimestamp [#^int i] (relation-field-get))
  (#^bytes .getBytes [#^int i] (relation-field-get))
  (#^String .getString [#^int i] (str (relation-field-get)))
  (#^int .getInt [#^int i] (int (relation-field-get))))


;;fjoin: (select * from R, table f(R) where r.x = f.x)

;; derby sql "reflection"

(defn tables
  "return a relation of :name, :definition of all tables in the current derby db
  connection."  
  []
  (let [;; TABLEID 	CHAR 	36 	false 	unique identifier for table or view
        ;; TABLENAME 	VARCHAR 	128 	false 	table or view name
        ;; TABLETYPE 	CHAR 	1 	false 	'S' (system table), 'T' (user table), 'A' (synonym), or 'V' (view)
        t (relation 'sys.systables 'tableid 'tablename 'tabletype)
        c (relation 'sys.syscolumns 'referenceid 'columnname 'columndatatype)
        s (relation 'sys.sysconstraints)]
    (join t c (join-condition = 'tableid 'referenceid))))

