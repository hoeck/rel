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
            [hoeck.rel.operators :as op]
            ;[hoeck.rel.iris :as iris]
            [hoeck.rel.structmaps :as st]
            [hoeck.rel.conditions :as cd]
            [hoeck.rel.testdata :as td]
            hoeck.magic-map.MagicMap
            hoeck.value-mapped-map.ValueMappedMap)
  (:use hoeck.library
        hoeck.magic-map
        hoeck.rel.conditions
        clojure.contrib.def))

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

(def-lots-of-aliases
  (op rename xproduct make-relation fields index) ;; union intersection difference
  ;(iris with-empty-universe <- ?-)
  )

;; test

(def people (op/make-relation td/people))


;; rename

(defn as
  "rename all fields of a relation such that they have a common prefix"
  [R prefix]
  (op/rename R (zipmap (fields R) (map #(keyword (str prefix "-" (name %))) (fields R)))))


;; select

(defn select*
  [R condition]
  (op/select R condition))

(defmacro select 
  ;; todo: pattern-like matching, eg: [a ? b] matches (condition (and (= *0 a) (= *2 b)))
  ;;       or {:name 'mathias} matches, computes to ->  #{[1 mathias weiland] [2 mathias probst]} == (condition (= *name 'mathias))
  ;;       maybe with nested, destructuring-like expressions like {:name #{mathias, robert}}
  ;;       or [? #{mathias, robert} dresden]
  ;;       or [(< 10 ?) ? (not= #{dresden, rostock})]
  ;;        -> use all of {[()]} 
  ;;       multiarg-select: keyword value -> hashmap access like `get' where (name :keyword) == field-name
  ;;       => qbe ????
  "Macro around select."
  [R condition]
  `(select* ~R (cd/condition ~condition)))


;; project

(def project* op/project) ;; list of keywords or conditions

(defmacro project
  "Convienience macro for the project operation."
  [R & exprs]
  `(project* ~R (list ~@(map #(cond (op/field? %)
                                      %
                                    (vector? %)
                                      `(cd/condition ~(first %) ~(second %))
                                    :else 
                                       `(cd/condition ~%))
                             exprs))))

;; union, difference, intersection

(defn make-set-op-fn
  "Return a function which extends the given 2-argument set function
  to take more than 2 relations."
  [set-op]
  (fn me 
    ([R] R)
    ([R S] (set-op R S))
    ([R S & more]
       (apply me (set-op R S) more))))

(def union (make-set-op-fn op/union))
(def intersection (make-set-op-fn op/intersection))
(def difference (make-set-op-fn op/difference))


;; joins

(defn make-join-op-fn [join-op]
  "return a function which implements the given join-op for more than two relation-field pairs."
  (fn multiarg-join    
    ([R r S s]
       (join-op R r S s))
    ([R r S s & more]
       (if (= 1 (count more)) (throw (java.lang.IllegalArgumentException. "wrong number of arguments to join")))
       (apply multiarg-join (join-op R r S s) r more))))

(def join (make-join-op-fn op/join))
(def right-outer-join (make-join-op-fn (fn [R r S s] (union (op/join R r S s) R))))
(def outer-join (make-join-op-fn (fn [R S r s] (union (join R r S s) (join S s R r) R S))))

(defn group-by
  "Return a hasmap of field-values to sets of tuples
  where they occur in relation R. (aka sql-group by or index)."
  ([R field & more-fields]
     (let [gi (fn group-index [index-set fields]
                (if-let [field (first fields)]
                  (magic-map (fn ([] (map field index-set))
                                 ([k] (group-index (clojure.set/intersection index-set (((index R) field) k)) (next fields)))))
                  index-set))]
         (magic-map (fn ([] (keys ((index R) field)))
                        ([k] (gi (((index R) field) k) more-fields)))))))


;; pretty printing

(defn- determine-column-sizes
  "Given a relation R, return a list of column-sizes according to opts."
  [R opts]
  (let [max-col-widths (map #(pipe (project* R (list %))
                                   (map pr-str)
                                   (map count)
                                   (map (partial + -1))
                                   (reduce max))
                            (fields R))
        pretty-col-widths (pipe max-col-widths
                                (map (partial min (:max-colsize opts 70)))
                                (map (partial max (:min-colsize opts 0))))
        small-fields-count (count (filter (partial <= (:min-colsize opts 0)) pretty-col-widths))
        amount (if (< small-fields-count 0)
                 (/ (- (reduce + pretty-col-widths) (:max-linesize opts 80))
                    small-fields-count)
                 0)]
    (zipmap (fields R) (if (< 0 amount)
                         (map #(max (:min-colsize opts) (- % amount)) pretty-col-widths)
                         pretty-col-widths))))

(def *pretty-print-relation-opts* {:max-lines 30, :max-colsize 30, :max-linesize 70 :min-colsize 1})

(defn pretty-print-relation
  "Print a relation pretty readably to :writer (default *out*), try 
  to align fields correctly while not to exceeding :max-linesize and
  other opts."
  [R & opts]
  (cond (= (count (fields R)) 1)
          (print (set R))
        :else        
          (let [opts (as-keyargs opts (assoc *pretty-print-relation-opts* :writer *out*))
                sizes (determine-column-sizes R opts)
                pr-field (fn [tuple field-name comma]
                           (let [v (get tuple field-name)
                                 s (sizes field-name)]
                             (str (str-cut (str field-name " "
                                                (str-align (str (pr-str v) (if comma "," ""))
                                                           (- s (count (str field-name)) (if comma 1 2))
                                                           (if (or (string? v) (symbol? v) (keyword? v)) :left :right))
                                                )
                                           s)
                                  (if comma " " ""))))
                pr-tuple (fn [tuple] (str "{" (apply str (map (partial pr-field tuple)
                                                              (fields R)
                                                              (concat (drop 1 (fields R)) '(false)))) "}"))]
            (binding [*out* (:writer opts)]
              (print"#{")
              (print (pr-tuple (first R)))
              (doseq [r (next R)]
                (println)
                (print (str "  " (pr-tuple r))))
              (println "}")))))
  
;; establish default pretty-printing of relations, needs awareness for *print-** variables
(defmethod print-method  hoeck.rel.Relation
  [R, w]
  (pretty-print-relation R :writer w))


;; sql stuff needs hoeck.rel
;(require '[hoeck.rel.sql-utils :as sql-utils])
;(require '[hoeck.rel.sql :as sql])

;(def-lots-of-aliases 
;  (sql-utils default-derby-args default-sybase-args))

;(defaliases
;  sql-connection sql-utils/make-connection-fn)


(comment

;; example relations: namespace, symbols

(def namespaces (make-relation 
                 (map #(list (ns-name %) %) (all-ns))
                 :fields [:name :namespace]))

(defn- ns-relation [ns-func field-name]
  (let [fields (list :ns-name :name field-name)]
    (make-relation (mapcat (fn [tup] (map #(zipmap fields (cons (:name tup) %)) 
                                          (ns-func (:namespace tup))))
                           namespaces)
                   :fields fields)))

(def namespace-aliases (ns-relation ns-aliases :alias))
(def namespace-imports (ns-relation ns-imports :import))
(def namespace-refers (ns-relation ns-refers :varname))
(def namespace-interns (ns-relation ns-interns :varname))
(def namespace-publics (ns-relation ns-publics :varname))

(= (select (difference namespace-interns namespace-publics) (= ~ns-name 'hoeck.rel.structmaps))
   (get-in (index (difference namespace-interns namespace-publics)) [:ns-name 'hoeck.rel.structmaps]))

;; example relations: files

(defn file-relation [path]
  (make-relation (map (fn [f] {:name (.getName f)
                               :path (.getParent f)
                               :size (.length f)})
                      (filter #(not (.isDirectory %)) (file-seq (java.io.File. path))))
                 :fields [:name :path :size]))

(op/order-by
 (project (file-relation "/home/timmy-turner/clojure/ra/hoeck")
          [(/ ~size 1000.0) :size-in-kb] :name :path)
 :size-in-kb
 '<)

(def classes (make-relation #{}))
(def methods )
(def constructors )
(def fields )

(defn class-tuple [c]
  {:name (symbol (.getName c))
   :type (cond (.isInterface c) :interface
               (.isEnum c) :enum
               (.isArray c) :array
               (.isAnonymousClass c) :anonymous
               (.isLocalClass c) :local
               (.isMemberClass c) :member
               (.isPrimitive c) :primitive
               (.isSynthetic c) :synthetic
               :else :class)
   :super (if-let [n (.getSuperclass c)]
            (symbol (.getName n)))})

(defn- add-class [c]
  (let [new-class (class-tuple c)
        sup (map class-tuple (supers c))]
    (union classes (make-relation (set (conj sup new-class))))))
(class-tuple (type {}))
{:import java.lang.ProcessBuilder, :name ProcessBuilder, :ns-name swank.core.threadmap}

(count (select people (= ~id 99)))

(binding [classes (make-relation (set (list (class-tuple java.lang.Object))))] 
  (add-class (type {})))

)



