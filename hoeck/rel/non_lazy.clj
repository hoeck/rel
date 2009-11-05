
;; eager evaluating implementation of the relational operators

(ns hoeck.rel.non-lazy
  (:use hoeck.rel.operators
	hoeck.rel.core
	hoeck.library)
  (:require [clojure.set :as set]))

(defmethod project :eager
  ([_] (emtpy-relation))
  ([R exprs]
     
     ))



