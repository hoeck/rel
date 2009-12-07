
(ns hoeck.rel.sql.create
  (:use [hoeck.rel.sql :only [sql-symbol sql-execute]]
        clojure.contrib.pprint
        clojure.contrib.macro-utils
        clojure.contrib.except
	;;clojure.contrib.sql ;; *db*
	[clojure.contrib.sql.internal :only [*db*]]))


;; create statement

(def sql-keyword-map
     ;; maps symbols to sql syntax
     (merge '{not-null "not null"
              empty-string "''"
              nil "null"
              unique "unique"
              ;; datatypes
              time "time",
              clob "clob",
              varchar "varchar",
              timestamp "timestamp",
              long-varchar "long varchar",
              xml "xml",
              date "date",
              float "float",
              numeric "numeric",
              smallint "smallint",
              integer "integer", int "integer"
              varchar-for-bit-data "varchar for bit data",
              char "char",
              long-varchar-for-bit-data "long varchar for bit data",
              bigint "bigint",
              decimal "decimal",
              double "double",
              blob "blob",
              real "real",
              char-for-bit-data "char for bit data"}
            (let [sql-keyw '()]
              (zipmap sql-keyw
                      (map str sql-keyw)))))

(defn generated [& [start increment]]
  (cl-format nil "generated by default as identity (start with ~a, increment by ~a)"
             (or start 0) (or increment 1)))

(declare column-def)
(defn varchar [len] (cl-format nil "varchar(~a)" len))
(defn decimal [prec & [scale]] (cl-format nil "decimal(~a,~a)" prec (or scale 5)))
(defn default [default-value] (cl-format nil "default ~a" (column-def default-value)))
(defn primary-key [& columns] (cl-format nil "primary key (~{~a~^, ~})" (map name columns)))

(defn column-def
  "given a symbol, a function or a list thereof, tries to produce the column
  part of a sql create-table statement."
  ([s]
     (cond (fn? s) (column-def (s)) ;; call
           (var? s) (column-def (s)) ;; call (assume a user-defined function)
           (seq? s) (let [[f & r] s ;; call or interpret
                          f-resolved (and (symbol? f) 
                                          (not (contains? sql-keyword-map f));; don't call eg. clojure.core/int
                                          (resolve f))]
                      (cond (or (fn? f) (var? f)) (column-def (apply f r));; call functions
                            f-resolved (column-def (apply f-resolved r));; call only non-sql-keyword vars
                            :else (cl-format nil "~{~a~^ ~}" (apply column-def s))))
           ;; shortcut-symbol or resolve and call without args
           (symbol? s) (let [number-symbol-rxs [[#"^varchar-([0-9]+)$" "varchar(%s)"]
                                                [#"^decimal-([0-9]+)$" "decimal(%s)"]
                                                [#"^decimal-([0-9]+)-([0-9]+)$" "decimal(%s,%s)"]]
                             sql-sym (sql-keyword-map s)]
                         (or sql-sym
                             (->> number-symbol-rxs
                                  (map (fn [[r fmt]]
                                         (if-let [[_ & n] (re-matches r (str s))]
                                           (apply format fmt n))))
                                  (remove nil?)
                                  first)
                             (when-let [rs (resolve s)] (column-def rs))
                             (throwf "unknown column-def symbol: %s" s)))
           ;; name (of column or constraint)
           (keyword? s) (sql-symbol (name s))
           ;; print-only
           (number? s) (str s) ;; str
           ;; identity           
           (string? s) s
           :else (str s)))
  ([s & more]
     (map column-def (cons s more))))

(defn create-table*
  "given a table name (symbol, string) and a table definition, return a 
  \"create table\" sql statement."
  [table-name & args]
  (cl-format nil "create table ~a (~{~a~^, ~})"
             (sql-symbol (name table-name))
             (map column-def args)))

;; some predefined columns

(defn text
  ([name] (list 'text name 5000))
  ([name len] (list name (list 'varchar len) 'not-null '(default empty-string))))

(defn string
  ([name] (list 'nonempty-string ))
  ([name len] (list name (list 'varchar len) 'not-null '(default empty-string))))

(defn id [name]
  (list name 'int 'not-null '(generated 1000)))

(defn lookup-id [name]
  (list name 'int 'not-null 'generated))

(defmacro create-table [name & definitions]
  `(create-table* '~name ~@(mapcat (fn [k] `('~k)) definitions)))

(comment
  ;; normal table
  (create-table person (:id int) (:name varchar-100) (primary-key :id :name))
  ;; predefined types
  (create-table status (id :id) (string :name 50) (text :doc 1000) (primary-key :id)))


;; drop

(defn drop-table [name]
  (cl-format nil "drop table ~s" (sql-symbol name)))

(comment
  (sql-execute (create-table person
                             (id :id)
                             (:status int)
                             (:name varchar-100)
                             (primary-key :id :status)))
  )