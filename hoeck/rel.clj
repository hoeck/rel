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

(ns hoeck.rel
  (:require ;[hoeck.rel.sql  :as sql]
            ;[hoeck.rel.sql-utils  :as sql-utils]
            [hoeck.rel.core :as rel-core]
            [hoeck.rel.iris :as iris]
            [hoeck.rel.fn])
  (:use hoeck.library
        clojure.contrib.def
        clojure.contrib.fcase))

(defmacro defaliases [& name-orig-pairs]
  `(do ~@(map #(list `defalias (first %) (second %)) (partition 2 name-orig-pairs))))

(defmacro def-lots-of-aliases 
  "Define aliases from namespace-name/sym to sym.
  body syntax: (namespace-name symbols-to-alias-here*)*"
  [& body]
  `(do ~@(mapcat (fn [[ns & lots-of-aliases]]
                   (map (fn [sym]
                          `(defalias ~sym ~(symbol (str ns) (str sym))))
                        lots-of-aliases))
          body)))

;; form the relation interface & some helpers
(defn as
  "rename all fields of a relation such that they have a common prefix"
  [R prefix]
  (rel-core/rename R (mapcat #(list % (symbol (str prefix "-" %))) (rel-core/fields R))))

(defn select-fn
  [R condition]
  (rel-core/check-fields! R (second (condition)))
  (rel-core/select R condition))

(defmacro select 
  ;; todo: pattern-like matching, eg: [a ? b] matches (condition (and (= *0 a) (= *2 b)))
  ;;       or {:name 'mathias} matches, computes to ->  #{[1 mathias weiland] [2 mathias probst]} == (condition (= *name 'mathias))
  ;;       maybe with nested, destructuring-like expressions like {:name #{mathias, robert}}
  ;;       or [? #{mathias, robert} dresden]
  ;;       or [(< 10 ?) ? (not= #{dresden, rostock})]
  ;;        -> use all of {[()]} 
  ;;       multiarg-select: keyword value -> hashmap access like `get' where (name :keyword) == field-name
  ;;       => qbe ????
  "convienience macro around select."
  [R condition]
  `(select-fn ~R (rel-core/condition ~condition)))

(defn join
  "multiple-relation join"
  ([R S r s] (rel-core/join R S r s))
  ([R S r s & more] (if (<= 3 (count more))
                      (let [[T j t & more] more] 
                        (apply join (rel-core/join R S r s) T j t more))
                      (throw (java.lang.IllegalArgumentException. "wrong number of arguments to join")))))

(defn group-by
  "Return a hasmap of field-values to sets of tuples 
  where they occur in relation R. (aka sql-group by or index)
  sets/ and tuples may be removed if not necessary (one-element set/tuple)."
  ([R field]
     (let [fpos (pos (rel-core/fields R) field)
           dtup #(let [v (if (map? %1)
                           (dissoc %1 field)
                           (rel-core/dissoc-vec %1 fpos))]
                   (if (= (count v) 1) (v 0) v))]
       (into {} (map (fn [[k v]] [k (let [v (map dtup v)]
                                      (if (rest v) v (first v)))])
                     ((rel-core/index R) fpos)))))
  ([R field & more-fields]
     (unsupported-operation!)
     ;;; ????
     ))

(defn tuples->maps
  "Returns a relation that contains maps instead of vectors as tuples.
  so #{[1 willy]} will become #{{id 1 name willy}}."
  [R]
  (let [fields (rel-core/fields R), index (rel-core/index R)
        ;;sm (apply create-struct fields)
        ;;tup->map #(apply struct-map sm (mapcat list fields %))
        tup->map #(apply hash-map (mapcat list fields %))
        index-fn (fn index-fn
                   ([i] (into {} (map (fn [[k v]] [k (set (map tup->map v))]) (index i))))
                   ([i v] (set (map tup->map  (index v i)))))]
    (rel-core/fproxy-relation (merge ^R {:index index-fn})
     {'count (fn count [_] (count R))
      'seq (fn seq [_] (map tup->map R))
      'get (fn get [_ k] (.get R (vec (vals k))))})))

;;; -> query by example; Applicable as a default .get method in fproxy-relation
(defn qbe 
  "Query by Example, utilizes the relational algebra."
  [R & args] ;; -> :name 'frieda :id 1
  (unsupported-operation!))
;  (cond (vector? (first args))
;          (query-by-tuple R (first args))
;        :else
;          (query-by-hashmap R (apply hash-map args))))
;; --> query-by-hashmap, query-by-tuple

(def-lots-of-aliases
  (rel-core project rename xproduct union intersection difference make-relation fields)
  (iris with-empty-universe <- ?-))


;; sql stuff needs hoeck.rel
(require '[hoeck.rel.sql-utils  :as sql-utils])
(require '[hoeck.rel.sql  :as sql])

(def-lots-of-aliases 
  (sql-utils default-derby-args default-sybase-args))

(defaliases
  sql-connection sql-utils/make-connection-fn)

