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
              :factory createRelation
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
;(require 'hoeck.rel.Relation)
;(binding [*compile-path* "/home/timmy-turner/clojure/classes"] (compile 'hoeck.rel.Relation))
;(binding [*compile-path* "g:\\clojure\\classes"] (compile 'hoeck.rel.Relation))
;(def xxx (hoeck.rel.Relation. {} {}))
;(def xxx (hoeck.rel.Relation. {} {'count (fn [_] 3) 'seq (fn [_] (map #(do (println %) %) '(a b c d e f g)))}))
;(def yyy (with-meta xxx {:meta 'data}))


;(doc gen-class)
; -------------------------
; clojure.core/gen-class
; ([& options])
; Macro
;   When compiling, generates compiled bytecode for a class with the
;   given package-qualified :name (which, as all names in these
;   parameters, can be a string or symbol), and writes the .class file
;   to the *compile-path* directory.  When not compiling, does
;   nothing. The gen-class construct contains no implementation, as the
;   implementation will be dynamically sought by the generated class in
;   functions in an implementing Clojure namespace. Given a generated
;   class org.mydomain.MyClass with a method named mymethod, gen-class
;   will generate an implementation that looks for a function named by 
;   (str prefix mymethod) (default prefix: "-") in a
;   Clojure namespace specified by :impl-ns
;   (defaults to the current namespace). All inherited methods,
;   generated methods, and init and main functions (see :methods, :init,
;   and :main below) will be found similarly prefixed. By default, the
;   static initializer for the generated class will attempt to load the
;   Clojure support code for the class as a resource from the classpath,
;   e.g. in the example case, ``org/mydomain/MyClass__init.class``. This
;   behavior can be controlled by :load-impl-ns

;   Note that methods with a maximum of 18 parameters are supported.

;   In all subsequent sections taking types, the primitive types can be
;   referred to by their Java names (int, float etc), and classes in the
;   java.lang package can be used without a package qualifier. All other
;   classes must be fully qualified.

;   Options should be a set of key/value pairs, all except for :name are optional:

;   :name aname

;   The package-qualified name of the class to be generated

;   :extends aclass

;   Specifies the superclass, the non-private methods of which will be
;   overridden by the class. If not provided, defaults to Object.

;   :implements [interface ...]

;   One or more interfaces, the methods of which will be implemented by the class.

;   :init name

;   If supplied, names a function that will be called with the arguments
;   to the constructor. Must return [ [superclass-constructor-args] state] 
;   If not supplied, the constructor args are passed directly to
;   the superclass constructor and the state will be nil

;   :constructors {[param-types] [super-param-types], ...}

;   By default, constructors are created for the generated class which
;   match the signature(s) of the constructors for the superclass. This
;   parameter may be used to explicitly specify constructors, each entry
;   providing a mapping from a constructor signature to a superclass
;   constructor signature. When you supply this, you must supply an :init
;   specifier. 

;   :methods [ [name [param-types] return-type], ...]

;   The generated class automatically defines all of the non-private
;   methods of its superclasses/interfaces. This parameter can be used
;   to specify the signatures of additional methods of the generated
;   class. Static methods can be specified with #^{:static true} in the
;   signature's metadata. Do not repeat superclass/interface signatures
;   here.

;   :main boolean

;   If supplied and true, a static public main function will be generated. It will
;   pass each string of the String[] argument as a separate argument to
;   a function called (str prefix main).

;   :factory name

;   If supplied, a (set of) public static factory function(s) will be
;   created with the given name, and the same signature(s) as the
;   constructor(s).
  
;   :state name

;   If supplied, a public final instance field with the given name will be
;   created. You must supply an :init function in order to provide a
;   value for the state. Note that, though final, the state can be a ref
;   or agent, supporting the creation of Java objects with transactional
;   or asynchronous mutation semantics.

;   :exposes {protected-field-name {:get name :set name}, ...}

;   Since the implementations of the methods of the generated class
;   occur in Clojure functions, they have no access to the inherited
;   protected fields of the superclass. This parameter can be used to
;   generate public getter/setter methods exposing the protected field(s)
;   for use in the implementation.

;   :exposes-methods {super-method-name exposed-name, ...}

;   It is sometimes necessary to call the superclass' implementation of an
;   overridden method.  Those methods may be exposed and referred in 
;   the new method implementation by a local name.

;   :prefix string

;   Default: "-" Methods called e.g. Foo will be looked up in vars called
;   prefixFoo in the implementing ns.

;   :impl-ns name

;   Default: the name of the current ns. Implementations of methods will be looked up in this namespace.

;   :load-impl-ns boolean

;   Default: true. Causes the static initializer for the generated class
;   to reference the load code for the implementing namespace. Should be
;   true when implementing-ns is the default, false if you intend to
;   load the code via some other method.
