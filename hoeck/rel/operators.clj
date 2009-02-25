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
(defn op-dispatch-fn [relation & rest]
  (:relation-tag ^relation))

(defn two-op-dispatch-fn [R, S & rest]
  (let [tag-r (:relation-tag ^R)
        tag-s (:relation-tag ^S)]
    (or (or (nil? tag-r) (nil? tag-s))
        (and (= tag-r tag-s) tag-r)
        :clojure)))

(defmulti project op-dispatch-fn)
(defmulti select op-dispatch-fn)
(defmulti rename op-dispatch-fn)
(defmulti xproduct two-op-dispatch-fn)
(defmulti join two-op-dispatch-fn) ;; right inner join
(defmulti union two-op-dispatch-fn)
(defmulti difference two-op-dispatch-fn)
(defmulti intersection two-op-dispatch-fn)
(defmulti order-by op-dispatch-fn)

;; default dispatch
(defmacro def-default-method
  "Expands to a defmethod form which defines the default dispatch method (on true) for 
  the given rel-algebra method name taking R-arg-count num args."
  [name R-arg-count]
  (let [rel-args (repeatedly gensym)]
    `(defmethod ~name true [~@(take R-arg-count rel-args) & args#]
       (apply ~'project ~@(map (fn [argname] `(make-relation ~argname)) (take R-arg-count rel-args)) args#))))

(comment
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;"primary keys" in relations:
; internal: "primary index"
; how to calculate the union with other sets?
#{[a 1] [b 2]} U #{[b 3]} -> #{[a 1] [b 2] [b 3]}
; but with #0 as primary-key
#{[a 1] [b 2]} U #{[b 3]} -> either #{[a 1] [b 2]} or #{[a 1] [b 3]} ??  ;-> merge ?

; consequence of pkey: 
[b 1] == [b 2] == [b ?x]
; whereas without
[b 1] != [b 2] != [b ?X]
; NO!

; affects: all ops except select
;project: remove the pkey tag if pkey field is not projected
;rename: rename field attr map too
;xproduct, join:  add new pkey if S has pkey
;set-ops: work only on the `primrary' key

; mhh, constraints (pkeys here) do really matter only on "inserts"
; rel. operations don't need them (yes, maybe for optimizing but thats beyond the scope of my impl.)

; first try: field attributes in metadata
; ex: (make-relation hoeck.rel.test/people-literal :fields '[#^{:primary-key true} id name vorname address-id])

; other approaches: (make-relation bla :fields '[id ...] :primary-key [id])

(make-constrained-relation ...)
;-> internal: {id [name city]}
;external: implements clojure.lang.IPersistentSet or hoeck.rel.Relation

;pseudo-clojure:
;(let [data '{1 [franz kafka], 
;             2 [frank codd],
;             3 [erich kaestner]}
;      index-fn (fn [] (if `index-of-id-wanted') data, normal index on "set" of data)]
;(fproxy-relation `[meta :index index-fn :tag clojure-constrained-relation]'
;  {'seq (fn [] (map #(vector `(val %) (key %) (val %)') data))
;   'count (fn [] (count data))
;   'contains (fn [tup] (if (data `id-extracted-from-tup') true false))
;   'get (fn [tup] like^^^^^)})

; [. * .]
; [* * *] <- selection
; [. * .]
;    ^
;    `------ projection
; =====================
; [*]


; [* * *] [* *]
; [* * *] [* *] <- join ^= concat of tuples
; [* * *] [* *]
; =====================
; [* * * * *]
; [* * * * *]
; [* * * * *]


; [* * *]
; [* * *]
;  U,D,I
; [* * *]
; [* * *]
; ======= set operation ^= concat of rows
; [* * *]
; [* * *]
; [* * *]

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;,

;tuple implementation
;;;;;;;;;;;;;;;;;;;;;
;sets of (struct-)maps
;the struct map:



;primitive functions

;project: remove certain keys from each structmap
;select: remove certain structmaps
;rename: rename struct-map-keys

)
