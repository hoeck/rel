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

(ns hoeck.rel.Relation
  (:gen-class :name hoeck.rel.Relation 
              :extends clojure.lang.APersistentSet
              :constructors {[clojure.lang.IPersistentMap clojure.lang.IPersistentMap] [clojure.lang.IPersistentMap clojure.lang.IPersistentMap]}
              ;;:factory createRelation
              :state "state"
              :init init))

(defmacro impl
  "get the function at key from the functions map "
  [key this] 
  `(~key (~'.state ~this)))

(defmacro def-method
  "Generate a method -name for all given arities which looks up its implementation fn in the map in (first .state)."
  [name & arities]
  (let [gs (repeatedly gensym)]
  `(defn ~(symbol (str "-" name))
     ~@(map (fn [c] (let [args (cons 'this (take c gs))]
                      `(~(vec args)
                      ((impl '~name ~(first args)) ~@args))))
              (if (empty? arities) '(0) arities)))))

(defn default-contains-fn
  [this key]
  (if (.get this key) true false))

(defn default-empty-fn [this]
  (with-meta #{} (assoc (.meta this) :index {})))

(def default-impl {'contains default-contains-fn
                   'empty default-empty-fn})

(defn -init [metadata fn-map]
  [[metadata {}] (merge default-impl fn-map)])

(defn -withMeta [this m]
  (hoeck.rel.Relation. m (.state this)))

(def-method count)
(def-method get 1)
(def-method seq)
(def-method contains 1)
(def-method empty)
(def-method cons 1)
(def-method disjoin 1)

;;;
;(require 'hoeck.rel.Relation :reload)
;(binding [*compile-path* "/home/timmy-turner/clojure/classes"] (compile 'hoeck.rel.Relation))
;(binding [*compile-path* "g:\\clojure\\classes"] (compile 'hoeck.rel.Relation))


