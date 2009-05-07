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

(ns hoeck.rel.iris
 (:require [hoeck.rel.operators :as op])
 (:use hoeck.library
       hoeck.rel))

;(add-classpath "file:///home/timmy-turner/clojure/iris-0.58.jar")
;(add-classpath "file:///home/timmy-turner/clojure/iris-parser-0.58.jar")
;(add-classpath "file:/g:/clojure/iris-0.58.jar")
;(add-classpath "file:/g:/clojure/iris-parser-0.58.jar")
(import '(java.util ArrayList)

        '(org.deri.iris KnowledgeBaseFactory)
        '(org.deri.iris.storage IRelation)
        '(org.deri.iris.api.basics ITuple IPredicate)
        '(org.deri.iris.api.terms IVariable INumericTerm IStringTerm IConstructedTerm)
        '(org.deri.iris.basics Tuple Predicate BasicFactory)
        '(org.deri.iris.terms TermFactory)
        '(org.deri.iris.terms.concrete ConcreteFactory)
        
        '(clojure.lang IObj)) ;; for meta & withMeta

;        '(hoeck.rel.iris IClojureSymbol IClojureKeyword))

(def persistent-exception
  (Exception. "this is a persisten object, therefor not implemented"))

;; IRIS API:

(def basic-factory (BasicFactory/getInstance))
(def term-factory (TermFactory/getInstance))
(def concrete-term-factory (ConcreteFactory/getInstance))

(defn term-create-symbol [form]
  (let [n (str form)]
  (proxy [hoeck.rel.iris.ISymbol] []
    (getValue [] n)
    (isGround [] true)
    (compareTo [x] (= n (.getValue x))))))

(defn variablename? [string]
  (.startsWith string "?"))

(defn make-term [form]
  (let [tf term-factory
        ctf concrete-term-factory]
    (cond (symbol? form)
            (let [n (name form)]
              (if (variablename? n)
                (.createVariable tf (.substring n 1))
                (.createString tf n)))
          (string? form)
            (.createString tf form)
          (integer? form)
            (.createInteger ctf form)
          (list? form)
            (.createConstruct tf 
                              (name (first form))
                              (map make-term (rest form))))))

(defn term->form
  "The opposite of make-term."
  [term]
  (instance-case term
    IVariable (symbol (str "?" (.getValue term)))
    hoeck.rel.iris.IClojureSymbol (symbol term)
    hoeck.rel.iris.IClojureKeyword (keyword term)
    IConstructedTerm (cons (symbol (.getFunctionSymbol term)) (map term->form (.getParameters term)))
    (.getValue term)))

(defn make-tuple [elements]
  (.createTuple basic-factory (ArrayList. (map make-term elements))))

(defn tuple->form
  "Opposite of make-tuple."
  [tuple]
  (map term->form tuple))

;(tuple->form (make-tuple '(1 2 3 "a")))
;(make-term '(foo manfred (bar ?X 99)))

(defn make-predicate [pred-name arity]
  (.createPredicate basic-factory 
                    (if (string? pred-name) pred-name (name pred-name))
                    arity))


(defn make-simple-immutable-relation
  "First cut relation impl."
  [r-set]
  (let [r-set (set (map make-tuple r-set)) ;; for lookup
        r-vec (vec r-set)] ;; for index-access
    (proxy [IRelation] [] 
      (add [tuple] (throw persistent-exception))
      (addAll [relation] (throw persistent-exception))
      (contains [tuple] (if (r-set tuple) true false))
      (get [index] (nth r-vec index nil))
      (size [] (count r-set)))))


(defn make-lazy-immutable-relation
  "... from a clojure relation."
  [R]
  (let [R-vec (delay (vec R))]
    (proxy [IRelation] []
      (add [tuple] (throw persistent-exception))
      (addAll [relation] (throw persistent-exception))
      (contains [tuple] (if (R (tuple->form tuple)) true false))
      (get  [index] (make-tuple (nth (force R-vec) index)))
      (size [] (count R)))))


(defn make-empty-relation
  "make an empty mutable relation using two refs."
  []
  (let [v (ref [])
        s (ref #{})]
    (proxy [IRelation ] []
      (add [tuple] (dosync (commute v conj tuple)
                           (commute s conj tuple)))
      (addAll [tuples] (dosync (commute v #(reduce conj %1 %2) tuples)
                                       (commute s #(reduce conj %1 %2) tuples)))
      (contains [tuple] (if (@s tuple) true false))
      (get [index] (@v index))
      (size [] (count v)))))

;; dispatch on relation
(defn make-relation
  "Create an iris IRelation from the given data R which may be a set or a seq."
  ([] (make-empty-relation))
  ([R] (or (and (set? R) (make-lazy-immutable-relation R))
           (and (seq? R) (make-simple-immutable-relation R)))))

(defn make-fact
  "a fact is a named relation."
  ([predicate-name relation]
     [(make-predicate predicate-name (rel-core/arity relation))
      (make-relation relation)]))

;; iris -> hoeck.rel
(defmethod rel-core/make-relation IRelation
  [irel & args]
  ;;; mhhh, be lazy??
  (rel-core/fproxy-relation {:relation-tag :iris}
    {'contains (fn [_ tuple] (if (.contains irel (make-tuple tuple)) true false))
     'seq (fn [_] (map #(vec (tuple->form (.get irel %))) (range (.size irel))))
     'count (fn [_] (.size irel))
     'get (fn [_ tuple] (if (.contains irel (make-tuple tuple)) tuple nil))}))

;(make-fact 'p '#{[4 3] [2 3] [1 2] [a 4]})

;p(1,2).
;p(2,3).
;p(4,3).
;p('a',p(1,2)).

;(.getRules parser)
;#=(java.util.ArrayList. [#<Rule q(?x, ?y) :- p(?x, ?y), GREATER_EQUAL(?x, ?y).>])

;; datalog-native: q(?x,?y) :- p(?x,?y), not ?x >= ?y.
;; datalog-lisp: (<- (q ?x ?y) (p ?x ?y) (not (>= ?x ?y)))
;; (<- (foo ?x ?y) (not (bar ?x ?y)))

(defn make-atom [form]
  (let [r (rest form)
        f (first form)]
  (.createAtom basic-factory 
               (make-predicate f (count r))
               (make-tuple (rest form)))))

(defn negation??? [form]
  (and (coll? form) (= (first form) 'not) (second form)))

(defn make-literal [form]
  (let [neg-f (negation??? form)]
    (.createLiteral basic-factory 
                    (if neg-f false true)
                    (make-atom (or neg-f form)))))

(defn make-rule [form]
  (.createRule basic-factory 
               (ArrayList. (list (make-literal (first form)))) ; head
               (ArrayList. (map make-literal (rest form))))) ; body
;(make-rule '((friend ?p ?q) (ties ?p ?q 'friend)))

(defn make-query [forms]
  (.createQuery basic-factory (ArrayList. (map make-literal forms))))

(def empty-universe {:facts {} :rules []})
(def *universe* (ref empty-universe))

(defn alter-universe [key val]
  (dosync (commute *universe* #(assoc % key (conj (key %) val)))))

(defn clear-universe []
  (dosync (ref-set *universe* empty-universe)))

;@*universe*
;(set-universe :facts ['bak 'bar])

(defn <-fn
  "Add a fact (relation) or a rule (equation) to *universe*.
  Function: all predicates and variables must be quoted manually."
  [& args]
  (let [[head & body] args]
	(if (not body)
      ;; a fact (relation)
      (let [[name vals] head]
        (if (set? vals)
          (alter-universe :facts (make-fact name vals))
          (throw (Exception. "only sets allowed in facts"))))
      ;; a rule (equation)
      (alter-universe :rules (make-rule args)))))

(defn ?-fn
  "Reason about the current *universe* facts and rules.
  Variables and Predicates must be quoted manually."
  [& args]
  (let [q (make-query args)
        kb (KnowledgeBaseFactory/createKnowledgeBase (:facts @*universe*) (:rules @*universe*))]
    (rel-core/make-relation (.execute kb q))))

(defn quote-predicates-and-variables [form]
  (map (fn [l] `(list '~(first l) ;; quote predicate
                      ~@(map #(if (and (symbol? %) (variablename? (name %)))
                                `'~% ;; quote variables
                                %)
                             (rest l))))
       form))

(defmacro <-
  "Add a fact (relation) or a rule (equation) to *universe*.
  ex. fact: (<- (predicate-name data))
        where predicate-name is a symbol and data is a seq or set of vectors of the same arity.
  ex. rule: (<- rule-head rule-body*)
        where rule-head/rule-body is (predicate-name variables/literals*)
          where predicate-name is a symbol and variable is a symbol starting with \"?\""
  [& args]
  `(<-fn ~@(quote-predicates-and-variables args)))

(defmacro ?- [& args]
  "Reason about the current *universe* facts and rules"
  `(?-fn ~@(quote-predicates-and-variables args)))

(defmacro with-empty-universe [& body]
  `(binding [*universe* (ref empty-universe)]
     ~@body))

;@*universe*
;(clear-universe)
;(<- (people rel-test/people))
;(<- (ties rel-test/ties))
;(<- (friend '?p '?q) (ties '?p '?q 'friend))
;(<- (name '?id '?name) (people '?id '?name '?a '?b))
;(<- (project-sth '?x '?y) '?y)
;(?- (people ?id ?name 'robert ?_))
;(?- (name 2 ?n))
;(?- (friend ?id ?r-id) (name ?id ?name) (name ?r-id ?friend))


; (rel-core/fields rel-test/people)
; (require 'hoeck.rel.fn)
; (rel-core/fields rel-test/ties)
; (hoeck.rel.core/select rel-test/ties (rel-core/condition (= 'friend *type)))

;;;;;;;;;;;;;;;;;;;;;;;

(comment
;;; trying to parse
(def parser (org.deri.iris.compiler.Parser.))
(.parse parser
"
p(1,2).
p(2,3).
p(4,3).
p('a',p(1,2)).

q(?x,?y) :- p(?x,?y), ?x >= ?y.

?-q(?x,?y).
")


(def fff (.getFacts parser))

(list-all-methods (class (first fff)))
(hashCode equals toString getValue getKey setValue getClass wait wait wait notify notifyAll)

(let [f (first fff)]
  [(.getKey f)
   (.getValue f)])

(def vvv (.get (.getValue (first fff)) 1))
(defn relation-seq [r]
  (map #(.get r %) (range (.size r))))

 ;(.getValue (first (.getFacts parser)))
(relation-seq (-> parser .getFacts first  .getValue))
;(#=(org.deri.iris.basics.Tuple. [#<IntegerTerm 1> #<IntegerTerm 2>]) #=(org.deri.iris.basics.Tuple. [#<IntegerTerm 2> #<IntegerTerm 3>]) #=(org.deri.iris.basics.Tuple. [#<IntegerTerm 4> #<IntegerTerm 3>]) #=(org.deri.iris.basics.Tuple. [#<StringTerm 'a'> #<ConstructedTerm p(1,2)>]))

(def rrr (?- (p ?x ?_)))

(isa? (class rrr) IRelation)


)







