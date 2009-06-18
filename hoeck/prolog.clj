
(ns hoeck.prolog
  (:use [hoeck.library :only [ifn-arity, fproxy, throw-arg-if]]
        [clojure.contrib.except :only [throwf]]
        [clojure.contrib.def :only [defnk]]
        [clojure.set :only [difference]])
  (:import (alice.tuprolog Prolog Term Struct Var Library PrimitiveInfo)))

(def tuprolog-library-max-arity 10)

(defn anonymous-var? [expr]
  (= '_ expr))

(defn get-variable-name [expr]
  (and (symbol? expr)
       (let [name (name expr)]
             (or (and (-> name first .charValue Character/isUpperCase) name)
                 (and (< 1 (count name)) (.startsWith name "?") (.substring name 1))))))

(defn make-term [expr]
  (if (list? expr)
    (if (empty? expr) 
      (alice.tuprolog.Struct.) ;; empty list
      (let [[head & tail] expr]
        (if (symbol? head)
          (let [args (into-array Term (map make-term tail))]
            (alice.tuprolog.Struct. (name head) args))
          (throwf "first item in predicate form is not a predicate name (symbol) but: `%s' of type `%s'"
                  (print-str expr)
                  (print-str (type expr))))))
    (cond (float? expr) (alice.tuprolog.Double. expr)
          (integer? expr) (alice.tuprolog.Long. (long expr)) ;; may overflow
          (anonymous-var? expr) (alice.tuprolog.Var.)
          :else (if-let [varname (get-variable-name expr)]
                  (alice.tuprolog.Var. varname)
                  (cond (or (symbol? expr) (keyword? expr)) (alice.tuprolog.Struct. (name expr))
                        (string? expr) (alice.tuprolog.Struct. expr)
                        :else (throwf "Cannot create Term from `%s' of type `%s'" 
                                      (print-str expr) 
                                      (print-str (type expr))))))))


(defn- lib-generate-sig [name paramtype returntype]
  (map #(vector (symbol (format "%s_%s" name %))
                (vec (repeat % paramtype))
                returntype)
       (iterate inc 1)))

(defmacro gen-clojure-library-interface [name max-arity]
  `(gen-interface :name ~name
                  :methods ~(reduce conj                                                    
                                    (vec (take max-arity (lib-generate-sig 'predicate alice.tuprolog.Term Boolean/TYPE)))
                                    (vec (take max-arity (lib-generate-sig 'functor alice.tuprolog.Term alice.tuprolog.Term))))))

(gen-clojure-library-interface hoeck.prolog.ClojureLibrary tuprolog-library-max-arity)
;; (binding [*compile-path* "g:\\clojure\\classes"] (compile 'hoeck.prolog))


(defnk make-library
  "Generate a Tuprolog Library Class with _one_ implemented functor or predicate.
  a predicate returns a boolean and a functor returns a Term.
  Both take up to tuprolog-library-max-arity Term args. Throwing Exceptions in
  functors/predicates results in a fail.
   type: 'predicate or 'functor; predicate returns a boolean, a functor returns
         an alice.tuprolog.Term
   name: string or symbol, name of the predicate/functor for the prolog engine
   fn: which takes at least 2 arguments [this arg_0] and at most 
       tuprolog-library-max-arity + 1
   keys: :therory optional theory string (prolog-code), nil = no theory"
  [type name f :theory nil] 
  (let [types #{'predicate 'functor}]
    (throw-arg-if (not (types type)) "type must be one of %s but is: %s" types type))
  (let [arity (difference (ifn-arity f) #{0 1}) ;; ifn arity returns a set of arities, including :rest
        impl (map (fn [a] 
                      {:method-name (-> type (str "_" a))
                       :arity a
                       :key (str type "/" a) ;; the name as declarated in the interface-def
                       :name (str name "/" a) ;; the `synonym', normally mapped using .getSynonymMap
                       :type-id ({'directive PrimitiveInfo/DIRECTIVE,
                                  'predicate PrimitiveInfo/PREDICATE, 
                                  'functor PrimitiveInfo/FUNCTOR} type)
                       :type type
                       :fn f})
                  (map dec (if (:rest arity) ;; predicate/functor arity is (- function-arity 1), each function has to take 'this as the first param
                             (range 2 (inc tuprolog-library-max-arity))
                             arity)))
        ;; tuprolog uses reflection/j.l.r.Method to call into Libraries
        find-method (fn find-method [this name arity]
                      (-> this .getClass (.getDeclaredMethod name (into-array Class (take arity (repeat Term))))))
        ;; PrimitiveInfo(int type, String key, Library lib, Method m, int arity)
        ;; String key: "functor/N" "directive/N" "predicate/N"; 0 < N < tuprolog-library-max-arity
        primitive-info (fn primitive-info [this key i]
                         ;; this is the enclosing Library object
                         ;; key is either :key - use the method-name or :name - use the synonym
                         ;; i is a tuple from impl
                         (PrimitiveInfo. (:type-id i) (key i) this (find-method this (:method-name i) (:arity i)) (:arity i)))]
    (fproxy [Library hoeck.prolog.ClojureLibrary] []
            (merge (into {} (map #(vector (:method-name %) (:fn %)) impl))
                   {;; return a String[][]: [[prolog-name, method-name, functor/predicate]*]
                    "getSynonymMap" (fn [_] (into-array [(into-array [(str name) (str type) (str type)])]))
                    ;; generate the primitive-info lists manually; 
                    "getPrimitives" (fn [this]
                                      ;; List[]: [[PrimitiveInfo*] [PrimitiveInfo*] [PrimitiveInfo*]]
                                      ;;          directives,      predicates,      functors
                                      (let [pi (map #(primitive-info this :name %) impl)]
                                        (into-array java.util.List [(filter (memfn isDirective) pi)
                                                                    (filter (memfn isPredicate) pi)
                                                                    (filter (memfn isFunctor) pi)])))}
                   (if theory {"getTheory" (fn [_] theory)})))))


;; example
(let [pl (Prolog.)]
  (.loadLibrary pl (make-library 'predicate 'foo (fn [this x] (println "called with: " x) (boolean true))))
  (.getSolution (.solve pl "foo(A).")))


