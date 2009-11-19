
(ns hoeck.rel.fields
  (:use hoeck.rel.operators
        hoeck.rel.conditions))

;; relational operations on the fields (the signature) of relations
;; fields are kept in sets, fields are keywords
;; (or symbols ??? -> metadata, then these operations must keep metadata)

(defmethod project :field [R conditions]
  (let [projected-fields (set (map #(condition-meta % :name) conditions))]
    projected-fields))

(comment (project (with-meta #{:a :b :c} {:relation-tag :field})
                  (list (identity-condition :a)
                        (condition (+ :b 10)))))

