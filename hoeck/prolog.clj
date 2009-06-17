
(ns hoeck.prolog
  (:use [clojure.contrib.except :only [throwf]]
        [clojure.contrib.def :only [defnk]])
  (:import (alice.tuprolog Prolog Term Struct Var Library)))

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

(gen-clojure-library-interface hoeck.prolog.PTC 10)
;; (binding [*compile-path* "g:\\clojure\\classes"] (compile 'hoeck.prolog))

(defnk make-library
  [type name fn :theory nil]
  (fproxy [Library hoeck.prolog.PTC] []
    (predicate [])))

; (use 'hoeck.library)

; (fn-arity (fn ([a b c]) ([]) ([a]) ([a b c d e & r])))
; (fn-arity (fn ([a & r])))

; (fproxy [hoeck.prolog.ClojureLibrary] []
;    {'predicate_1 (fn [this arg] (list this arg))})


       


