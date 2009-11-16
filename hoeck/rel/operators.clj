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

(ns hoeck.rel.operators)

;; relational algebra operations

(defn type-dispatch [thing & opts]
  (type thing))

(defn op-dispatch-fn [relation & rest]
  (or (:relation-tag ^relation) :clojure))

(defn join-dispatch [R r, S s]
  (let [tag-r (:relation-tag ^R)
        tag-s (:relation-tag ^S)]
    (or (or (nil? tag-r) (nil? tag-s))
        (and (= tag-r tag-s) tag-r)
        :clojure)))

(defn two-op-dispatch-fn [R, S & rest]
  (let [tag-r (or (:relation-tag ^R) :clojure)
        tag-s (or (:relation-tag ^S) :clojure)]
    (if (= tag-r tag-s)
      tag-r
      :clojure)))

(defn many-op-dispatch-fn [& relations]
  (let [the-one (into #{} (map #(or (:relation-tag (meta %)) :clojure)
                               relations))]
    (if (= 1 (count the-one)) 
      (first the-one)
      :clojure)))

(defmulti project op-dispatch-fn)
(defmulti select op-dispatch-fn)
(defmulti rename op-dispatch-fn)
(defmulti xproduct two-op-dispatch-fn)

(defmulti join two-op-dispatch-fn) ;; natural-join
(defmulti outer-join two-op-dispatch-fn) ;; right-outer-join
(defmulti fjoin op-dispatch-fn) ;; join with a custom function

(defmulti union many-op-dispatch-fn)
(defmulti difference many-op-dispatch-fn)
(defmulti intersection many-op-dispatch-fn)

(defmulti order-by op-dispatch-fn)
(defmulti aggregate op-dispatch-fn)

;; constructor methods
(defmulti relation type-dispatch)
(defmulti fields op-dispatch-fn)


