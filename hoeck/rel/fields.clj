
(ns hoeck.rel.fields
  (:use hoeck.rel.operators
        hoeck.rel.conditions
	clojure.contrib.except)
  (:require [clojure.set :as set]))

;; relational operations on the fields (the signature) of relations
;; fields are kept in sets, fields are keywords
;; (or symbols ??? -> metadata, then these operations must keep metadata)
;; throw errors if nonexistend fields are used in conditions


(defn check-unknown-fields
  "given a set and a set/seq of fields, check wether all used-fields
  are contained in R, otherwise throw an Exception."
  [R used-fields]
  (let [unknown-fields (set/difference (->> used-fields (map keyword) set)
                                       (->> R (map keyword) set))]
    (when-not (empty? unknown-fields)
      (throwf "unknown fields: %s" unknown-fields))))

(defmethod project :field [R conditions]
  (let [cm (map #(condition-meta %) conditions)
	ineligible-conditions (remove #(or (#{:identity :user} %)
                                           (nil? %)) 
                                      (map :type cm))
        p-fields (set (map #(-> % :name name symbol) cm))]
    (check-unknown-fields R (mapcat :fields cm))
    (when-not (empty? ineligible-conditions)
      (throwf "Only :identity and :user conditions allowed in project, not: %s"
	      (print-str ineligible-conditions)))
    (set (filter p-fields R))))

(comment (project (with-meta #{:a :b :c} {:relation-tag :field})
                  (list (identity-condition :a)
                        (condition (+ ~b 10)))))

(defmethod select :field [R condition]
  (check-unknown-fields R (condition-meta condition :fields))
  R)

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

(defmethod join :field [R S join-condition]
  (let [cm (condition-meta join-condition)]
    (when-not (= :join (:type cm)) (throwf "Must be a join-condition"))
    (when-not (:join-symbol cm) (throwf "Unknown join function"))
    (check-unknown-fields R (list (:field-a cm)))
    (check-unknown-fields S (list (:field-b cm)))
    (when-not (empty? (set/intersection R S)) (throwf "Relation signatures overlap, use rename before joining.")) ;; ignore?
    (set/union R S)))

(comment (join (with-meta #{:a :b :c} {:relation-tag :field})
	       (with-meta #{:d :e :f} {:relation-tag :field})	       
	       (join-condition > :a :f)))

(defmethod fjoin :field [R f]) ;; too dynamic ???

(defmacro def-set-op [op-name]
  `(defmethod ~op-name :field [R# S#]
     (when-not (= R# S#)
       (throwf "The two relations are not union compatible, the fields in question are: %s"
	       (set/union (set/difference R# S#) (set/difference S# R#))))
     (set/union R# S#)))

(def-set-op union)
(def-set-op difference)
(def-set-op intersection)


(defmethod aggregate :field [R conditions]
  (let [cm (map #(condition-meta %) conditions)
        ineligible (remove #{:aggregate :identity} (map :type cm))]
    (when-not (empty? ineligible)
      (throwf "only :aggregate and :identity conditions allowed in aggregate, not %s"
              (print-str ineligible)))
    R))