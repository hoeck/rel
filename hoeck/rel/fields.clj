
(ns hoeck.rel.fields
  (:use hoeck.rel.operators
	clojure.contrib.except)
  (:require [clojure.set :as set]
            [hoeck.rel.expressions :as e]))

;; relational operations on the fields (the signature) of relations
;; fields are kept in sets, fields are keywords
;; (or symbols ??? -> metadata, then these operations must keep metadata)
;; throw errors if nonexistend fields are used in conditions


(defn check-unknown-fields
  "given a set and a set/seq of fields, check wether all used-fields
  are contained in R, otherwise throw an Exception."
  [R used-fields]
  (let [unknown-fields (set/difference (set used-fields)
                                       (set R))]
    (when-not (empty? unknown-fields)
      (throwf "unknown fields: %s" unknown-fields))))

(defmethod project :field [R exprs]
  (let [p-fields (map #(-> % name symbol) (map e/get-name exprs))]
    (set (map R p-fields))))

(comment (project (with-meta '#{a b c} {:relation-tag :field})
                  (list (e/identity-expr :a)
                        (e/expr (+ b 10)))))

(defmethod select :field [R expr] R)

(defmethod rename :field [R oldname-newname-map]
  (check-unknown-fields R (keys oldname-newname-map))
  (set (map #(if-let [nf (oldname-newname-map %)]
               (with-meta nf (meta %))
               %) 
            R)))

(comment (rename (with-meta #{:a :b :c} {:relation-tag :field})
		 {:a :x :c :y :c :v}))

(defmethod xproduct :field [R S]
  (union R S))

(defmethod join :field [R S join-expr]
  (set/union R S))

(defmethod fjoin :field [R f]) ;; too dynamic ???

(defmacro def-set-op [op-name]
  `(defmethod ~op-name :field [R# S#]
     ;;(when-not (= R# S#)
     ;;       (throwf "The two relations are not union compatible, the fields in question are: %s"
     ;;	       (set/union (set/difference R# S#) (set/difference S# R#))))
     (set/union R# S#)))

(def-set-op union)
(def-set-op difference)
(def-set-op intersection)

(defmethod aggregate :field [R exprs]
  (set (map #(let [n (-> % e/get-name symbol)]
               (get R n n))
            exprs)))

(defmethod order-by :field [R fields]
  
  )