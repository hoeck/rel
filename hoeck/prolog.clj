
(ns hoeck.prolog
  (:use [hoeck.library :only [ifn-arity, fproxy, throw-arg-if, jcall, pipe]]
        [clojure.contrib.except :only [throwf]]
        [clojure.contrib.def :only [defnk]]
        [clojure.set :only [difference]]
        clojure.contrib.test-is)
  (:import (alice.tuprolog Prolog Term Struct Var Library PrimitiveInfo Theory)))

(defn- anonymous-var?
  "Return true if expr is the anonymous-var-expression: '_"
  [expr]
  (= '_ expr))

(defn- get-variable-name
  "Return fals if expr is not a symbol representing a varname.
  Varnames are symbols with either a capital starting letter or
  starting with a question mark. Example:
  (map get-variable-name '(A ?A Bee ?bee (A) ? _ :x))
  -> (\"A\" \"A\" \"Bee\" \"bee\" false false false false)"
  [expr]
  (and (symbol? expr)
       (let [name (name expr)]
             (or (and (-> name first .charValue Character/isUpperCase) name)
                 (and (< 1 (count name)) (.startsWith name "?") (.substring name 1))))))

(defn- make-term 
  "Create a Tuprolog Term from a given s-expr."
  [expr]
  (if (seq? expr)
    (if (empty? expr) 
      (alice.tuprolog.Struct.) ;; empty list
      (let [[head & tail] expr]
        (if (or (= :- head) 
                (= '<- head)
                (= "," head)
                (symbol? head)) ;; :- denotes a clause in tuprolog
          (let [args (into-array Term (map make-term tail))]
            (alice.tuprolog.Struct. (if (= head '<-) ":-" (str head)) args))
          (throwf "first item in predicate form is not a predicate name (symbol or :-) but: `%s' of type `%s'"
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

(def #^{:private true} tuprolog-library-max-arity 10)

(defn- lib-generate-sig [name paramtype returntype]
  (map #(vector (symbol (format "%s_%s" name %))
                (vec (repeat % paramtype))
                returntype)
       (iterate inc 1)))

(defmacro #^{:private true} gen-clojure-library-interface
  "Generate an interface according to the tuprolog library documentation.
  boolean predicate_N ( Term{N} ) and 
  Term    functor_N   ( Term{N} ) for each N = 1 .. tuprolog-library-max-arity."
  [name]
  (let [max-arity tuprolog-library-max-arity]
    `(gen-interface :name ~name
                    :methods ~(reduce conj                                                    
                                      (vec (take max-arity (lib-generate-sig 'predicate alice.tuprolog.Term Boolean/TYPE)))
                                      (vec (take max-arity (lib-generate-sig 'functor alice.tuprolog.Term alice.tuprolog.Term)))))))

(gen-clojure-library-interface hoeck.prolog.ClojureLibrary)
;; (binding [*compile-path* "g:\\clojure\\classes"] (compile 'hoeck.prolog))


(defnk #^{:private true} make-library
  "Generate a Tuprolog Library Class with _one_ implemented functor or predicate.
  A predicate returns a boolean and a functor returns a Term.
  Both take up to tuprolog-library-max-arity Term args. Throwing Exceptions in
  functors/predicates results in a fail.
    type: 'predicate or 'functor; predicate returns a boolean, a functor returns
          an alice.tuprolog.Term
    name: string or symbol, name of the predicate/functor for the prolog engine
    fn: which takes at least 2 arguments [this arg_0] and at most 
        tuprolog-library-max-arity + 1
    keys: :theory optional theory string (prolog-code), nil = no theory
  Use (.loadLibrary (Prolog.) (make-library ...)) to add the generated library to the
  Tuprolog engine."
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
(comment (let [pl (Prolog.)]
           (.loadLibrary pl (make-library 'predicate 
                                          'foo 
                                          (fn [this x]
                                            (println "called with: " x)
                                            true)))
           (.getSolution (.solve pl "foo(A)."))))

(defn- read-term [str] ;; to debug and inspect how tuprolog creates terms
  (Term/createTerm str))

(defn- term-properties [t]
  (remove nil? (map #(if (try (jcall t (val %))
                              (catch Exception e nil))
                       (key %))
                    '{:compound isCompound,
                      :atomic isAtomic,
                      :emptylt isEmptyList,
                      :list isList,
                      :primitive isPrimitive,
                      :clause isClause,
                      :struct isStruct,
                      :atom isAtom,
                      :ground isGround,
                      :number isNumber,
                      :var isVar})))

;; A set of rules (as alice.tuprolog.Term)
;; `<-' and `?-' operate on that

(def *rules* (atom #{}))

(defn clear []
  (swap! *rules* (constantly #{})))

(defn <-* [rule]
  (swap! *rules* conj rule))

(defmacro <- [& body]
  `(<-* '~body))

(defn- make-clojure-term [term]
  (let [get-name (fn [] (-> term .getName symbol))]
    (condp instance? term
      alice.tuprolog.Number (condp instance? term
                              alice.tuprolog.Double (.doubleValue term)
                              alice.tuprolog.Float (.floatValue term)
                              alice.tuprolog.Long (.longValue term)
                              alice.tuprolog.Int (.intValue term))
      alice.tuprolog.Struct (let [get-terms (fn [] (map #(make-clojure-term (.getTerm term %)) (range 0 (.getArity term))))]
                              (cond (.isAtom term) (get-name)
                                    (.isClause term) (cons '<- (get-terms))
                                    (.isCompound term) (cons (get-name) (get-terms))
                                    (.isList term) (throwf "todo")))
      alice.tuprolog.Var (cond (.isAnonymous term) '_
                               :else (get-name)))))

;;example: (make-clojure-term (make-term '(:- (a X Y) (a 1 aaa bbb ccc))))

(defn- make-theory [terms]
  (Theory. (if (empty? terms)
             ""
             (Struct. (into-array terms)))))

(defn- term-from-rule
  [rule]
  (make-term (if (next rule) 
                       (cons :- rule) ;; a clause
                       (first rule))))

(defn- solve ;; return a lazy seq of SolveInfo answers
  ([query] (solve query @*rules*))
  ([query rules]
     (let [pr (Prolog.)]
       (.setTheory pr (make-theory (map term-from-rule rules)))
       (lazy-seq (let [si (.solve pr query)]
                   (if (.isSuccess si)
                     (cons si (take-while identity 
                                          (repeatedly #(when (.hasOpenAlternatives pr) 
                                                         (.solveNext pr)))))))))))

;(def solveinfo
;     (let [x (solve (make-term '(p 1 1)) (map make-term '((p 1 1) (p 2 1) (p 3 2))))]
;       ;;(.getName (first (.getBindingVars (first x))))
;       x))

(defn- solveinfo-results
  "Return a hashmap of variable->value from a given SolveInfo. Return
  a empty hashmap, if the solution binds no variables."
  [solveinfo]
  (if (or (nil? solveinfo) (not (.isSuccess solveinfo)))
    nil
    (apply hash-map
           (mapcat #(list (symbol (.getName %)) (make-clojure-term (.getTerm %)))
                   (.getBindingVars solveinfo)))))

(defn ?-* [& query-term] 
  (map solveinfo-results (solve (make-term (if (next query-term)
                                             (cons "," query-term)
                                             (first query-term)))
                                @*rules*)))

(defmacro ?- [& query]
  `(?-* '~@query))

;;;;
(clear)
(<- (p 1))
(<- (p 2))
(<- (p 4 4))
@*rules*
(<- (p X X) (p X) (p Y))
(?- (p X Y) (p X) (p Y))
(?- (p X X))
(?-* '(p X X))

