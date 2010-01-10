
(ns hoeck.library
  (:use [clojure.contrib.except :only [throw-arg]]))


(defn remove-interned-symbols [foreign-ns-name]
  (map (partial ns-unmap *ns*)
       (keys (ns-publics (find-ns foreign-ns-name)))))

(defmacro %>
  "Short for #(-> % ...)"
  [& body]
  `(fn [arg#] (-> arg# ~@body)))

;;; sequence stuff

(defn segment
  "Takes one collection and returns two collections of the same type with
  all elements where (pred item) returns true in the first collection and
  false items in the second collection."
  [pred coll]
  (reduce (fn [[p, non-p] item]
            (if (pred item)
              [(conj p item), non-p]
              [p, (conj non-p item)]))
          [(empty coll) (empty coll)]
          coll))

(defn in?
  "Return true if sequence s countains element e."
  [s e]
  (some #(= e %) s))


(defn
  #^{:test #(let [x '(1 2 3 a b c)]
             (and (= (apply interleave (deinterleave x)) x)
              (= (apply interleave (deinterleave x 3)) x)))
     :doc "Inverse function to `interleave', defaults to (= n 2), returns a vector of sequences."}
  deinterleave
  ([s] (deinterleave 2 s))
  ([n s] (vec (map #(take-nth n (nthnext s %)) (range n)))))

(defn partition-with
  "Returns a lazy sequence of lists by splitting the original seq on 
  elements where pred returns true. Except for the first list all nested
  lists have a matched element as their head."
  [pred seq]
  ((fn part-fn [c]
     (cond (nil? c) nil           
           :else (let [[f r] (split-with (complement pred) (next c))]
                   (lazy-seq (list* (first c) f) (part-fn r)))))
     seq))

(defn sqr [x]
  (* x x))

(defn pos
  "The opposite of get, returns the first index of a value in a seq."
  [coll value]
  (first (filter identity (map #(and (= value %) %2) (seq coll) (repeatedly inc 0)))))

;; taken from: http://groups.google.com/group/clojure/browse_thread/thread/66ff0b89229be894
(defmacro let->
   "Provide a name that will be bound to the result of the first form.
   For each additional form, the variable will be
   used in the invocation, and then rebound to the result of the form."
   [varname start & forms]
   (let [fn-args `[~varname]
         wrapped (map (fn [form] `(fn ~fn-args ~form)) forms)]
     (reduce
           (fn [acc func] `(~func ~acc))
           start
           wrapped)))
;; example: 
;;   (take 3 (filter odd? (range 1 20))) == (let-> X (range 1 20) (filter odd? X) (take 3 X))

(defn positive?
  "Return the number n if it is greater than zero, otherwise return nil"
  [n]
  (if (< 0 n) n nil))

(defn negative?
  "Return the number n if it is smaller than zero, otherwise return nil"
  [n]
  (if (< n 0) n nil))

(defn walk-expr
  "Maps a function over an expression by recuring over every coll?.
cut is a function which decides wether to recur into a collection form,
pred is a function which decides if f should be applied to the current form."
  [cut pred f expr]
  (if (cut expr)
    expr
    (let [new-expr (if (pred expr) (f expr) expr)]
      (if (coll? new-expr)
        (if (cut new-expr)
          new-expr
          (let [new-form (into (empty new-expr) (map (if (instance? clojure.lang.IPersistentMap new-expr)
                                                       #(vector (walk-expr cut pred f (key %)) (walk-expr cut pred f (val %)))
                                                       #(walk-expr cut pred f %))
                                                     new-expr))]
            (if (list? new-form) (reverse new-form) new-form)))
        new-expr))))

(defn replace-exprs
  "Replace exprs where pred returns true with value of (f exp)."
  [pred f expr]
  (walk-expr (fn [_] false) pred f expr))

(defn collect-exprs-recursively
  "collect all expressions and their subexpressions matching pred."
  [pred expr]
  (if (coll? expr)
    (concat (mapcat (if (instance? clojure.lang.IPersistentMap expr)
                      #(concat (collect-exprs-recursively pred (key %)) (collect-exprs-recursively pred (val %)))
                      #(collect-exprs-recursively pred %))
                    expr)
            (if (pred expr) (list expr)))
    (if (pred expr) (list expr))))

(defn collect-exprs
  "collect all expressions matching pred."
  [pred expr]
  (if (pred expr)
    (list expr)
    (if (coll? expr)
      (concat (mapcat (if (instance? clojure.lang.IPersistentMap expr)
                        #(concat (collect-exprs pred (key %)) (collect-exprs pred (val %)))
                        #(collect-exprs pred %))
                      expr)
              (if (pred expr) (list expr))))))
      
(defn collect-forms
  "Same as collect-expr but apply f only on the first element of list? epxressions."
  [f expr]
  (collect-exprs #(and (list? %) (f (first %))) expr))

(defn rand-nth
  "use rand-int to get an index to retrieve an element from src with nth."
  [src]
  (nth src (rand-int (count src))))

(defn throw-unsupported-operation!
  "throw a java.lang.UnsupportedOperationException with messages as text."
  [format-str & format-args]
  (throw (UnsupportedOperationException. (apply format format-str format-args))))

(defn throw-illegal-argument! 
  "throw a java.lang.IllegalArgumentException with messages as text."
  [format-str & format-args]
  (throw (IllegalArgumentException. (apply format format-str format-args))))

(defmacro do1
  "like do but return the result from the first form."
  [& body]
  `(let [result# ~(first body)]           
    ~@(next body)                         
    result#))


;;; string functions

(defn make-string
  "Make a String by concatenating string s n times."
  [s n]
  (apply str (take n (repeat s))))

(defn str-align
  "Align a string to the given length either :left :right or :center(ed).
  If s is bigger than size, just return s."
  ([s size] (str-align s size :right))
  ([s size align]
   (let [l (count s)
         space (make-string " " (- size l))] 
     (if (>= l size) s
         (condp = align
           :left  (str s space)
           :right (str space s)
           :center (let [n (/ (- size l) 2)
                         space-left (make-string " " (Math/floor  n))
                         space-right (make-string " " (Math/ceil  n))]
                     (str space-left s space-right)))))))

(defn str-cut
  "Return a string which is at least size long, append three dots if
  s is longer than size."
  ([s size] (str-cut s size "..."))
  ([s size more-string]
     (if (< size (count s))
       (if (< size (count more-string))
         (.substring more-string 0 size)  
         (str (.substring s 0 (- size (count more-string))) more-string))
       s)))

(defn printbar
  "print values in sequence s in a bar chart of height."
  [s height]
  (let [make-bar (fn [height value]
                   (concat (take (- height value) (repeat "  ")) (take value (repeat "# "))))
        scale (/ height (reduce max s))]
    (doall (map #(println ";;" %) (apply map str 
    (loop [heights (reverse (map #(.intValue (* scale %)) s))
           result ()]
        (if-let [h (first heights)]
                (recur (next heights) (cons (make-bar height h) result))
                result)))))
    (count s)))


;;; reflection

;; from rhickey: http://paste.lisp.org/display/67182
(defn jcall [obj name & args]
  (clojure.lang.Reflector/invokeInstanceMethod obj 
					       (str name)
					       (if args (to-array args) clojure.lang.RT/EMPTY_ARRAY)))

(defn jcallstatic [class-or-classname name & args]
  (clojure.lang.Reflector/invokeStaticMethod class-or-classname
					     (str name)
					     (if args (to-array args) clojure.lang.RT/EMPTY_ARRAY)))

(defn jfn [name]
  #(apply jcall %1 name %&))

(defn jctor [class & args]
  (clojure.lang.Reflector/invokeConstructor class
    (if args (to-array args) clojure.lang.RT/EMPTY_ARRAY)))

;((jfn 'substring) "fred" 2 3)
;((jfn 'toUpperCase) "fred") 

(defn get-class
  "Get a Class from x, use its name if x is a symbol or string, its value if
  its a class else use class x."
  [x]
  (cond (string? x)
          (.forName Class x)
        (symbol? x)
          (get-class (str x))
        (class? x)
          x
        :else
          (class x)))
           
(defn- all-methods
  "return a seq of all methods (java.lang.reflect.Method) that (get-class thing) supports."
  [& things]
  (mapcat #(-> (get-class %) .getMethods seq) things))

(defn list-all-methods
  "return a list of all method-names implemented by thing"
  [& things]
  (map #(symbol (.getName %)) (apply all-methods things)))

(defn list-method-args
  "Return a vector containing at least one list of arguments a method takes."
  [m & things]
  (let [meths (filter #(= (.getName %) (str m)) (apply all-methods things))]
    (map #(map (fn [t] (symbol (.getName t))) (seq (.getParameterTypes %))) meths)))

(defn class-exists?
  "Return true if class named by classname (a symbol or string) exists."
  [classname]
  (try
   (Class/forName (str classname))
   true
   (catch java.lang.ClassNotFoundException e
     false)))

(defn lisp-to-camelcase
  "Given a lisp-symbol or keyword, transform its name to CamelCase. Set first-letter-uppercase? to true
  to make it happen, otherwise the first letter will be lowercase."
  [first-letter-uppercase? symbol-or-keyword]
  (when symbol-or-keyword
    (let [sn (.split (name symbol-or-keyword) "-")
          upcase-first #(str (.toUpperCase (.substring % 0 1)) (.substring % 1))]
      (apply str (cons (if first-letter-uppercase? 
                         (upcase-first (first sn))
                         (first sn))
                       (map upcase-first (next sn)))))))

(defn lispify-camelcase
  "Given a CamelCased JavaSymbol, return a string with the camel-case replaced by lispy-dashes."
  [j]
  (let [m (re-matcher #"[A-Z][a-z]*" (str (.toUpperCase (.substring j 0 1)) (.substring j 1)))]
    (.toLowerCase (apply str (interpose "-" (take-while identity (repeatedly #(re-find m))))))))

(defn make-enumerator-fn
  "Given a n Enum Class, return a function that maps enums to keywords and vice versa.
  Calling the generated function without any arguments returns a seq of possible keys.
  Calling it with either an enum or keyword returns the correspondin keywod or 
  Throw illegal-argument-exceptions on inappropriate arguments."
  [enum-class]
  (when-not (isa? enum-class Enum) (throw-illegal-argument! enum-class " is not a java.lang.Enum"))
  (let [enums (jcallstatic enum-class "values")
	keys (map #(-> % .name .toLowerCase lispify-camelcase keyword) enums)
	k-e-map (zipmap keys enums)
	e-k-map (zipmap enums keys)]
    (fn enum
      ([] keys)
      ([key-or-enum]
	 (cond (instance? enum-class key-or-enum) (e-k-map key-or-enum)
	       (keyword? key-or-enum) (if-let [e (k-e-map key-or-enum)]
					e
					(throw-illegal-argument! "no enum for key: %s, possible keys are: %s"
								 key-or-enum (enum)))
	       :else (throw-illegal-argument! "Must be either a keyword or an Enum of type: %s"
					      (.getName enum-class)))))))

(defn ifn-arity
  "Return a set of numbers #{(0..19)*} or #{(0..19)* :rest} denoting valid arities/varargs
  of a clojure Function. Throws an IllegalArgumentException if (ifn? f) fails."
  [f]
  (when-not (ifn? f) (throw-arg "Expecting an IFn, got a: %s" (type f)))
  (->> (seq (.getDeclaredMethods (class f)))
       (filter #(#{"doInvoke" "invoke"} (.getName %)))
       (map #(if (= (.getName %) "doInvoke")
	       :rest
	       (count (.getParameterTypes %))))
       (set)))

(defn get-methods-by-name
  "get methods from class matching (str name)."
  [class method-name]
  (filter #(= (str method-name) (.getName %)) (seq (.getMethods class))))

(defn list-classpaths
  "Return a list of classpaths from the System/getProperty java.class.path."
  []
  (seq (.split (System/getProperty "java.class.path") (System/getProperty "path.separator"))))

(defn list-bootclasspaths
  "Return the bootclasspaths; sun-jvm only."
  []
  (if-let [bcp (System/getProperty "sun.boot.class.path")]
    (seq (.split bcp (System/getProperty "path.separator")))))


;;; maths

;;; jacobi method for solving linear equations
(defn- jacobi-x-step [m b x i]
  "Calculate a part of the solution of a jacobi-methos step"
  ;;       1
  ;; xi = --- (bi - (  sum       aij * xij)
  ;;      mij        (not= i j)
  (* (/ 1 (get-in m [i i])) (- (b i) (reduce + (map #(* (get-in m [i %]) (x %)) (remove #(= i %) (range (count x))))))))

(defn- jacobi-method-step
  "Calculate one step of the jacobi-method given a matrix m, a vector b, and a former solution x."
  ([m b] 
     ;; first step of the jacobi method, init x with nullvector
     (jacobi-method-step m b (vec (repeat (count b) 0))))
  ([m b x] (vec (map #(jacobi-x-step m b x %) (range (count x))))))

(defn jacobi-method
  "Return a lazy sequence of solutions to the equation of
  matrix m and vector b and x: M*b = x. To obtain a convergent sequence,
  m should be diagonally dominant.
  If no former solution x is given, it is initialized by the jacobi-step-method."
  ([m b] (jacobi-method-step m b))
  ([m b x] (iterate (partial jacobi-method-step m b) x)))

;;; example:
;(last (take 30
;            (jacobi-method 
;             [[3 1 1]
;              [1 3 1]
;              [1 1 3]]
;             [11.0 10.0 8.0]
;             )))


;;; misc

(defn file-map
  "Read all files matching regex from directory PATH.
  Return a map of keywords to files. Keywords are generated from
  the first matching group of regex and filename."
  [path regex]
  (->> (if (isa? path java.io.File) path (java.io.File. path))
       (.listFiles)
       (seq)
       (remove #(.isDirectory %))
       (map #(vector (-> (re-seq regex (.getName %)) first second) %))
       (remove #(-> % first nil?))
       (map #(vector (-> % first keyword) (second %)))
       (into {})))

(defn messagebox [& stuff]
  "Display arguments in a swing messagebox"
  (let [s (apply str (map (fn [x] (str x " ")) stuff))]
    (. javax.swing.JOptionPane (showMessageDialog nil s))
    (first stuff)))


(defn bit-set? [bit int]
  (not (zero? (bit-and (bit-shift-left 1 bit) int))))