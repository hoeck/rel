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


;;; +---------------------+
;;; | library for clojure |
;;; +---------------------+

(ns hoeck.library
  (:use [clojure.contrib.except :only [throw-arg]]))

;;; +---------------------+               
;;; | basic lisp stuff    |               
;;; +---------------------+               


;(defn function?
;  "test wether x implements clojure.lang.IFn."
;  [x] (instance? clojure.lang.IFn x))
;;; -> ifn?


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

(defn split-every
  "Split sequence s every n elements, returning a lazy seq of lists.
Defaults to (= n 2)."
  ([s] (split-every s 2))
  ([s n]
     (map #(take n (nthnext s %))
          (range 0 (count s) n))))

(def rowsn split-every)

;; -> clojure.core/partition
(defn
  #^{:test #(let [x '(1 2 3 a b c)]
             (and (= (apply interleave (unzip x)) x)
              (= (apply interleave (unzip x 3)) x)))
     :doc "Inverse function to `interleave', defaults to (= n 2)."}
  unzip
  ([s] (unzip 2 s))
  ([n s] (map #(take-nth n (nthnext s %)) (range n))))

(def partition-nth unzip)

(defn even-elements "Return al even elements of a seq, eql to (take-nth 2 s)." [s] (take-nth 2 s))

(defn odd-elements "Return al odd elements of a seq, eql to (take-nth 2 (next s))." [s] (take-nth 2 (next s)))

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

;;; flatten one level of the list: (flat1 '((1) (2 3) (4))) -> (1 2 3 4)
(defn flat1
  "flatten one level of the seq:
  (flat1 '(1 (2 (3)) (4))) -> (1 2 (3) 4)"
  [seq]
  (apply concat (map #(if (seq? %) % (list %)) seq)))

;;; obsoloete (now included)    
; (defn read-string
;   "read lisp-objects from a string str"
;   [str]
;   (read (new java.io.PushbackReader (new java.io.StringReader str))))

(defn find-next
  "find-next: return the next of seq where fn for the head returns true.
  (find-next (fn [x] (= x 'foo)) '(bar foo bak)) -> (foo bak)"
  [f seq]
  (drop-while (fn [x] (not (f x))) seq))

;;;  1 (1 2 3) -> (3 1 2)
;;; -1 (1 2 3) -> (2 3 1)
(defn shift-right-circulate [n l]
  (loop [n n
         r l]
    (if (== n 0) r
      (if (< n 0)
          (recur (+ n 1)
                 (concat (next r) (list (first r))))
        (let [rev (reverse r)]
          (recur (- n 1)
                 (cons (first rev) (reverse (next rev)))))))))
;;(shift-right-circulate -1 [1 2 3 4])

(defn shift-left-circulate [n l]
  (shift-right-circulate (* n -1) l))
;;(shift-left-circulate 1 '(1 2 3))

;(defn not-empty?
;  "Return nil if s is the empty list, otherwise return s."
;  [s]
;  (if (= s ()) nil s))
;;; use (seq X) instead

(defn adjust-seq
  "add elements to s from a-seq or remove elements from s in order
  to adjust s to the length of a-seq.
  ex.: (adjust-seq [25] [1 2 3]) -> (25 2 3)
       (adjust-seq [1 2 3] [25]) -> (1)
       (adjust-seq '(1 2 3) '(7 8 9) -> (1 2 3)"
  [s a-seq]
  (let [len-s (count s)
        len-as (count a-seq)]
    (cond (== len-s len-as) s
          (< len-s len-as) (concat s (drop len-s a-seq))            
          :else (take len-as a-seq))))

(defn cons-keep-count
  "Cons element onto list discarding the last element of list.
  Ex.: (cons-keep-count 0 '(1 2 3)) -> (0 1 2)"
  [element list]
  (cons element (take (- (count list) 1) list)))

(defn sqr [x]
  (* x x))

(defn make-comparator
  "make a java.util.Comparator out of a fn.
  fn must accept 2 args at least.
  fn should return true if o1 and o2 are in order"
    [f]
  (proxy [java.util.Comparator] []
         (compare [o1 o2] (if (f o1 o2) -1 1))))

(defn sort-by-seq
  "sort seq-0 using compare-fn and apply the order of seq-0 to seq-1
  Return Seq-1. See (doc make-comparator) for description of compare-fn.
  Example: (sort-by-seq < '(1 4 3 2) '(a b c d)) -> (a d c b)"
    [compare-fn seq-0 seq-1]
  (let [s (map vector seq-0 seq-1)]
    (map second (sort (make-comparator #(compare-fn (nth %1 0) (nth %2 0)))
                      s))))

(defn counter
  "like range but always infinite. Defaults to (= start step 0)."
  ([] (iterate inc 0))
  ([start] (iterate inc start))
  ([start step] (iterate #(+ % step) start)))

(defn pos
  "The opposite of get, returns the first index of a value in a seq."
  [coll value]
  (first (filter identity (map #(and (= value %) %2) (seq coll) (counter)))))

(defn single? [k] (when (not (next k)) (first k)))

(defn assoc-meta
  "Assoc key-value(s) on objects metadata."
  [obj & keyvals]
  (with-meta obj (apply assoc ^obj keyvals)))

;; taken from: http://groups.google.com/group/clojure/browse_thread/thread/66ff0b89229be894
(defmacro pipe
  "Threads the expr through the forms. Inserts x as the
  last item in the first form, making a list of it if it is not a
  list already. If there are more forms, inserts the first form as the
  last item in second form, etc."
  ([x form] (if (seq? form)
              `(~(first form) ~@(next form) ~x)
              (list form x)))
  ([x form & more] `(pipe (pipe ~x ~form) ~@more)))
;; example: 
;;   (take 3 (filter odd? (range 1 20))) == (pipe (range 1 20) (filter odd?) (take 3))


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

(defmacro throw-arg-if [test & args]
  `(when ~test (throw-arg ~@args)))

;(defn atom?
;  "Returns logical true if x is an atom. An atom here is every expression
;  which is not a readable clojure datastructure."
;  [x]
;  (not (or (instance? (class '[v e c t o r])     x)
;           (instance? (class '#{s e t})          x)
;           (instance? (class '(l i s t))         x)
;           (instance? (class '{:h a :sh m :a p}) x))))

;(defn walk-expr
;  "Maps a function over an expression by recuring over every readable datastructure.
;The function must accept an expression as an argument and return a vector: [use-new-expr?, new-expr]
;On use-new-expr?, the current expression is replaced with new-expr, otherwise, new-expr is ignored."
;  [f expr]
;  (let [[use-new-expr?, new-expr] (f expr)]
;    (if use-new-expr? new-expr
;      (cond (instance? (class '[vector]) expr)
;          (vec (map #(walk-expr f %) expr))
;        (instance? (class '#{set}) expr)
;          (set (map #(walk-expr f %) expr))
;        (instance? (class '(list)) expr)
;          (apply list (map #(walk-expr f %) expr))
;        (instance? (class '{:persistent hash :map 1}) expr)
;          (apply hash-map (mapcat #(list (walk-expr f (key %)) (walk-expr f (val %))) expr))
;        :else
;          expr))))




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
      

;(defmacro symbol-macrolet [names-expansions body]
;  (let [ne (apply hash-map names-expansions)]
;    (walk-expr
;     #(vector (ne %) (ne %))
;     body)))

;(defn collect-expr
;  "Walk expr and return a list of all subexpressions where pred returns true."
;  [pred expr]
;  (with-local-vars [result []]
;    (walk-expr #(vector false, (if (pred %) (var-set result (conj (var-get result) %)))) expr)
;    (var-get result)))

;(defn collect-expr
;  "Walk expr and return a list of all subexpressions where pred returns true."
;  [pred expr]
;  (filter pred (flatten-bf (walk-expr-reverse #(if (pred %) %) expr))))

(defn collect-forms
  "Same as collect-expr but apply f only on the first element of list? epxressions."
  [f expr]
  (collect-exprs #(and (list? %) (f (first %))) expr))

(defn rand-nth
  "use rand-int to get an index to retrieve an element from src with nth."
  [src]
  (nth src (rand-int (count src))))

(defn hashm
  "inspired by vec: short for (apply hash-map s)."
  [s]
  (apply hash-map s))

(defn as-keyargs
  "Turn elements in arglist into a hashmap. If the length of arglist is odd then
  expect the last argument to be a hashmap and merge it with previously generated hm.
  Use this to accept keyword args in an & rest form of a fn and supply optionally a 
  default-args map.
  Optionally you may supply an initial hashmap for the keyword args. This way you can
  define default keys in the function."
  ([arglist] (as-keyargs arglist {}))
  ([arglist initial-hashmap]
     (loop [args arglist
            result-map initial-hashmap]
       (let [pair (take 2 args)
             rest (drop 2 args)]
         (cond (= (count pair) 2)
                 (recur rest (conj result-map [(first args) (second args)]))
               (= (count pair) 1)
                 (let [last-arg (first pair)]
                   (when-not (map? last-arg) (throw (IllegalArgumentException. "Last argument is not a map")))
                   (merge last-arg result-map))
               :else
                 result-map)))))

      
(defn unsupported-operation!
  "throw a java.lang.UnsupportedOperationException with messages as text."
  [& messages]
  (throw (UnsupportedOperationException. (apply str messages))))

(defn illegal-argument! 
  "throw a java.lang.IllegalArgumentException with messages as text."
  [& messages]
  (throw (IllegalArgumentException. (apply str messages))))

;;; ------
;;; macros 
;;; ------

; (defmacro setr
;   "set refs in a transaction, changes to refs are immediately visible to
;   the other forms in the setr.
;   ex: (let [x (ref 1)
;             y (ref 2)]
;         (setr x 'a y @x)
;         (list @x @y)) -> (a a)"
;   [& ref-newvalue-pairs]
;   `(sync nil
;     ~@(apply map (fn [ref value] (list 'ref-set ref value)) (partition-nth ref-newvalue-pairs))))

; (defmacro psetr
;   "like setr but calculate the values outside the transaction and set all refs at once"
;   [& ref-newvalue-pairs]
;   (let [[refs values] (partition-nth ref-newvalue-pairs)
;         gensyms (map gensym (str refs))]
;     `(let ~(apply vector (apply concat (map #(list %1 %2) gensyms values)))
;       (setr ~@(apply concat (map #(list %1 %2) refs gensyms))))))

; ;;; functions for accessing ref'd hashmaps
; (defmacro sethash
;   "Set key to val in a ref'd hashmap using (assoc ...)."
;   [hashmap & key-val]
;   `(psetr ~hashmap (assoc (deref ~hashmap) ~@key-val)))

; (defn remhash
;   "Remove key from ref'd hash-map."
;   [hashmap key]
;   (psetr hashmap (dissoc @hashmap key)))

; (defn gethash 
;   "Get key from ref'd hash-map. Optionally supply a
;   default value if (get hashmap key) returns nil."
;   ([hashmap key]
;    (get @hashmap key))
;   ([hashmap key default]
;    (if-let [val (get @hashmap key)] val default)))

; (defn conjhash
;   "Conj value to hash-set at key in hash-map."
;   [hashmap key value]
;   (sethash hashmap key (conj (gethash hashmap key #{}) value)))

(defmacro do1
  "like do but return the result from the first form."
  [& body]
  `(let [result# ~(first body)]           
    ~@(next body)                         
    result#))

(defmacro unlazy-map [& rest] `(doall map ~@rest))

(defn symbol-name
  "like the cl symbol-name
on keywords without `:'"
  [s]
  (if (keyword? s)
      (. (str s) (substring 1)) ;; remove the `:'
      (str s)))

(defmacro empty?->nil
  "Expands to a let expression that returns nil if thing 
  is an empty? coll? and otherwise returns thing."
  [thing]
  `(let [maybe-coll# ~thing]
     (if (and (coll? maybe-coll#) (empty? maybe-coll#)) nil maybe-coll#)))



(defn read-relation-matrix-literal
  "read a matrix literal and return a hashmap mapping pairs of columnnames
  to cell values, useful for literal definitions of sets, functions, relations.
  The name of the relation is stored as the results metadata.
  See hoeck.library/sample-matrix-relation for an example."
  [src]
  (let [[heading, body] (split-with (complement #(re-matches #"={3,}" (str %))) (remove #(or (= % '|) (= % '||)) src))
        relation-name (first heading)
        horiz-cols (next heading)
        horiz-arity (count horiz-cols)
        rows (into {} (map #(vector (first %) (next %)) (partition (inc horiz-arity) (next body))))
        result (reduce #(into % (map (fn [c h] [[(key %2) h] c]) (val %2) horiz-cols)) {} rows)]
    (with-meta result {'relation-matrix-literal-name relation-name})))

(defmacro relation-matrix-literal 
  "Expand into a form which calls read-relation-matrix-literal with the
  given body. Do the neccesary quoting of `|', `||' and `===*' (table drawing characters).
  See hoeck.library/sample-matrix-relation for an example."
  [& body]
  (let [quoted-form
          (cond (and (list? body) (= (first body) 'quote))
                  body
                :else
                  (map #(if (and (symbol? %) (or ('#{| ||} %) (re-matches #"={3,}" (str %))))
                          (list 'quote %)
                          %)
                       body))]
    `(read-relation-matrix-literal (list ~@quoted-form))))

(def sample-matrix-relation
     (let [a :alpha
           b :beta]            
       (relation-matrix-literal
         'my-name || a | b
         ==================
         a        || a | a
         b        || a | b)))


(defmacro eval-when
  "Eval body only when compile-time condition is true."
  [condition & body]
  (when (eval condition)
    `(do ~@body)))



; (defn keyword-to-symbol
;   "make a symbol from the corresponding keyword"
;   [keyword]
;   (read-string (symbol-name keyword)))

;;; +-----------------+
;;; | Pretty Printing | ;; broken
;;; +-----------------+

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



;;;; java.lang.System.out.println
;;(defn println [& r]
;;  (. (. System out) (println
;;                     (reduce strcat (map str r)))))

;;;; print object to String
;(defn prints [x]
;  (let [string-writer (new java.io.StringWriter)]
;    (. clojure.lang.RT (print x string-writer))
;    (. string-writer (toString))))

;;;; pretty-print object to string
;;;; for now, print 'x instead of (quote x)
;(defn pprints [x]
;  (if (and (seq? x) (not (instance? clojure.lang.PersistentHashMap x)))
;      (if (= (first x) 'quote)
;          (strcat "'" (pprints (second x)))
;          (strcat (if (instance? clojure.lang.PersistentVector x) "[" "(")
;                  (loop [o x
;                           s ""]
;                      (if o (recur (rest o) (strcat s (pprints (first o)) (if (frest o) " " ""))) s))
;                  (if (instance? clojure.lang.PersistentVector x) "]" ")")))
;          (prints x)))
;;;; (pprints '(a '(b 'c) d)) -> "(a '(b 'c) d)" ;; instead of (prints '(a '(b 'c) d)) -> "(a (quote (b (quote c))) d)"

;;;; for tables and trees
;(defn pprints-aligned
;  ([object size] (pprints-aligned pprints-aligned object size :right))
;  ([prn-fn object size align]
;   (let [o (prn-fn object)
;         l (. o (length))
;         space (make-string " " (- size l))]
;     (if (>= l size) o
;         (case align
;           :left  (strcat o space)
;           :right (strcat space o)
;           :center (let [n (/ (- size l) 2)
;                           space-left (make-string " " (.floor Math n))
;                           space-right (make-string " " (.ceil Math n))]
;                     (strcat space-left o space-right)))))))

;(defn print-str-a
;  "Print to string aligned.
;  Align :center, :left or default :right.
;  Example: (print-str-a \"foo\" 10) -> \"       foo\""
;  ([object size] (print-str-a object size :right))
;  ([object size align] (pprints-aligned print-str object size align)))


;;;; pretty tree printer for interactive (x)emacs development
;(defn pretty-print-tree
;  ;;; root
;  ;;  |-- elem
;  ;;  |   |--
;  ;;  |   `--
;  ;;  |--
;  ;;  `--
;  ([seq]
;   (pretty-print-tree seq ";; "))
;  ([seq prefix]
;   (loop [s seq]
;       ;;(let [;;prefix (reduce strcat (take level (cons "" (repeat "|   "))))]
;       (if (seq? (first s))
;           (pretty-print-tree (first s) (strcat prefix (if (rest s) "|   " "    ")))
;           (println (strcat
;                 prefix
;                 (if (not (second s))
;                     "`-- "
;                     (if (and (seq? (second s)) (not (second (rest  s)))) "`-- " "|-- " ))
;                 (str (first s)))))
;     (when (rest s) (recur (rest s))))))

;              O
;           /  |  \
;          1   3   4
;        / | \     | \
;       2  3  4    A  B 
;            /|\   |
;           5 6 7  X

(defn flatten-df
  "Returns a depth-first lazy sequence of tree t."
  [t] (mapcat #(if (coll? %) (conj (flatten-df (next %)) (first %)) (list %)) t))

;;(flatten tree)
(defn flatten-bf
  "Returns a breadth-first lazy sequence of tree t."
  [t] (when t (lazy-cat (map #(if (coll? %) (first %) %) t)
                        (flatten-bf (mapcat #(if (coll? %) (next %) nil) t)))))




;;; get the 'first' row and count -> table size
;;; hint: used for (x)emacs 
;(defn pretty-guess-table-colsize [str]
;  (if (string? str)
;      (let [str (. str (trim))
;            first-newline (. str (indexOf "\n"))]
;        (if (<= first-newline 1)
;            1 ;; default
;            (count (read-string (strcat "(" (. str (substring 0 first-newline)) ")")))))
;      ;; default:
;      1))

;;; example: a 2x3 (rows x cols) table
;; table: (aaaaa b cc d e fff)
;; rows:  ((aaaaa b cc)
;;         (d e fff))
;; cols:  ((aaaaa d) (b e) (cc fff))
;; formatted: (aaaaa b cc
;;             d     e fff)
;(defn pretty-print-table
;    ([table] (pretty-print-table table nil))
;    ([table flattenp]
;         (let [;; get rows
;               rows table
;               ;; number of columns (max)
;               col-len (max (map count rows))
;               ;; make rows of equal len (col-len)
;               rows-x (map (fn [x] (take col-len (concat x (repeat "")))) rows)
;               ;; get cols
;               cols (apply map list rows-x)
;               ;; get a list of the maximum col sizes
;               max-col-sizes (map (fn [x] (+ 1 (max (map (fn [x] (. (pprints x) (length))) x)))) cols)
;               ;; write the table cells (as a seq)
;               cells-list (loop [r rows
;                                 result nil]
;                              (if (not r)
;                                  result
;                                  (recur (rest r)
;                                         (concat result (list (loop [row (first r)
;                                                                     sizes max-col-sizes
;                                                                     cur-result nil]
;                                                                  (if (not row)
;                                                                      cur-result
;                                                                      (recur (rest row)
;                                                                             (rest sizes)
;                                                                             (concat cur-result (list (pprints-aligned (first row) (first sizes) :left)))))))))))]
;           ;; write the list, including newlines
;           (apply strcat (unlazy-map (if flattenp
;                                         (fn [x] (strcat ( . (apply strcat x) (trim)) "\n"))
;                                         (fn [x] (strcat "(" ( . (apply strcat x) (trim)) ")\n")))
;                                     cells-list)))))

;;; examples:
;;; (println (pretty-print-table '((a bbb c) (ddd e))))
;;;->
;;; (a   bbb c)
;;; (ddd e)
;;;
;;; (println (pretty-print-table '((a bbb c) (ddd e)) :t))
;;;->
;;; a   bbb c
;;; ddd e

;;(snip-n seq col-len)
;(defn pretty-print-table-string [str col-len]
;  (pretty-print-table (rowsn (read-string (strcat "(" str ")")) col-len) :t))

;;; convert JavaStyleNames to lisp-style-names
(defn pretty-print-java [src]
    (let [p (. java.util.regex.Pattern (compile "[A-Z]"))
          m (. p (matcher (str src)))
          s (new StringBuffer)]
      ;;; now we have the classname, convert java to lisp style
      (. m (find 1))
      (loop []
          (when (. m (find))
            (. m (appendReplacement s "-$0"))
            (recur))) ;; note: while loop!
      (. m (appendTail s))
      (. (str s) (toLowerCase))))

;; turn `class javax.swing.JButton' into some more lispy-readable eq:
;; `j-button' or `class javax.awt.GridLayout' into `grid-layout'
(defn pretty-print-class [c]
  ;;; get the classname without the package in front
  (let [class-string (str (. c (getClass)))
        pattern (. java.util.regex.Pattern (compile "\\.[a-zA-Z0-9]+$"))
        matcher (. pattern (matcher class-string))]
    (if (. matcher (find))
        (pretty-print-java (. class-string (substring (+ 1 (. matcher (start))) (. matcher (end)))))
        class-string)))

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

;;; -------------------------------
;;; general java interop/reflection
;;; -------------------------------

(defn file-map
  "Read all files matching regex from directory PATH.
  Return a map of keywords to files. Keywords are generated from
  the first matching group of regex and filename."
  [path regex]
  (pipe (if (isa? path java.io.File) path (java.io.File. path))
        (.listFiles)
        (seq)
        (remove #(.isDirectory %))
        (map #(vector (-> (re-seq regex (.getName %)) first second)
                      %))
        (remove #(-> % first nil?))
        (map #(vector (-> % first keyword) (second %)))
        (into {})))



;;; from rhickey: http://paste.lisp.org/display/67182
(defn jcall [obj name & args]
  (clojure.lang.Reflector/invokeInstanceMethod obj (str name)
    (if args (to-array args) clojure.lang.RT/EMPTY_ARRAY)))

(defn jfn [name]
  #(apply jcall %1 name %&))

(defn jctor [class & args]
  (clojure.lang.Reflector/invokeConstructor class
    (if args (to-array args) clojure.lang.RT/EMPTY_ARRAY)))

;((jfn 'substring) "fred" 2 3)
;((jfn 'toUpperCase) "fred") 


;;; messagebox:
;;; useful for side effect testing
(defn mb [& stuff]
  "Display arguments in a swing messagebox"
  (let [s (apply str (map (fn [x] (str x " ")) stuff))]
    (. javax.swing.JOptionPane (showMessageDialog nil s))
    (first stuff)))

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
           
(defn all-methods
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

(defn make-enumerator [enum-class & enum-keywords]
  "make a function which takes an enum-class as input and returns the
  corresponding keyword. The symbol-name of the keywords must match the
  enums exactly (up/downcase).
  The function also takes keywords as input and produces Enums as output."
  (let [enum-classes (apply hash-map (flat1 (map #(list (.valueOf Enum enum-class (symbol-name %)) %) enum-keywords)))]
    (fn [thing]
        (cond (instance? Enum thing)
                (get enum-classes thing)
              (keyword? thing)
                (.valueOf Enum enum-class (symbol-name thing))))))

;; proxy-with-obj-version-1
(defmacro proxy-with-obj
  "Create a proxy from class-or-interface (preferably from an interface)
  and implement all methods using the objects methods except the methods given
  in body (same format as in clojure/proxy)."
  [class-or-interfaces, obj & body]
  (let [obj# obj
        arg-gensyms (repeatedly gensym)]
    `(proxy ~class-or-interfaces []
       ~@(concat (filter #(not-any? (fn [x] (= % x)) (map first body))
                (distinct (map #(let [method-name (symbol (.getName %))
                                      args (take (min 19 (count (seq (.getParameterTypes %)))) arg-gensyms)]
                                  `(~method-name ~(vec args)
                                     (. ~obj# ~method-name ~@args)))
                               (apply all-methods class-or-interfaces))))
                 body))))

;; proxy-with-obj-version-2
(defn generate-proxy-function-expr [proxy-obj-expr method-name & argcounts]
  (let [this-gensym (gensym "this")
        arg-gensyms (repeatedly gensym)]
    `(fn ~@(map #(let [args (take % arg-gensyms)]
                   (list (vec (cons this-gensym args)) `(. ~proxy-obj-expr ~method-name ~@args)))
                argcounts))))

(defmacro fn-proxy-with-obj-core
  [expr-gen-fn class-and-interfaces, obj & method-name-fn-pairs]
  (let [gobj (gensym "obj")]        
  `(let [~gobj ~obj
         pc# (get-proxy-class ~@class-and-interfaces)
         p# (construct-proxy pc#)]
    (update-proxy p# ~(merge
                       ;; auto-generated method-fns
                       (into {} (map #(vector `(quote ~(symbol (.getName %))) (expr-gen-fn gobj (symbol (.getName %)) (count (.getParameterTypes %)))) (apply all-methods class-and-interfaces)))
                       ;; supplied method-fns
                       (into {} (map #(vector `(quote ~(first %)) (second %)) (split-every method-name-fn-pairs)))))
    p#)))

(defmacro fn-proxy-with-obj
  "Create a proxy from class-or-interface (preferably from an interface)
and implement all methods using the objects methods except the methods
defined here. Define methods as functions.
CURRENTLY SUPPORTS ONLY ONE CLASS OR INTERFACE!"  
  [class-or-interface obj & body]
  `(fn-proxy-with-obj-core ~generate-proxy-function-expr [~class-or-interface] ~obj ~@body))

(defmacro fn-proxy-with-delayed-obj
  "Like fn-proxy-with-obj but wrap obj in a delay and force it on the first attempt
to access this proxies autogenerated methods."
  [class-or-interface obj & body]
  `(fn-proxy-with-obj-core ~(fn [e m & r] (apply generate-proxy-function-expr `(force ~e) m r))
                        [~class-or-interface] (delay ~obj) ~@body))

;;example:
(comment
(let [x (fn-proxy-with-delayed-obj clojure.lang.IPersistentVector (do (println "forced") (vec [1 2 3]))
                                count (fn [this] 1)
                                nth (fn [this _] 0))]
  (println "made proxy:" x)
  (println "forcing proxy: " (conj x 1)))
)

;;; pre rev 1159 fproxy version:
;;(defn fproxy
;;  "like proxy but not a macro and take a hashmap: {method-name method-fn, ...}
;;  ex.: (.count (fproxy [clojure.lang.IPersistentVector] [] {'count (fn [this] 99)})) -> 99."
;;  [class-and-interfaces constructor-args method-name-fn-map]
;;  (let [proxy-class (apply get-proxy-class class-and-interfaces)
;;        concrete-proxy (apply construct-proxy proxy-class constructor-args)]
;;    (update-proxy concrete-proxy method-name-fn-map)
;;    concrete-proxy))

(defmacro fproxy
  "like proxy but take a hashmap: {method-name method-fn, ...}
  ex.: (.count (fproxy [clojure.lang.IPersistentVector] [] {'count (fn [this] 99)})) -> 99."
  [class-and-interfaces constructor-args method-name-fn-map]
  `(let [pc# (proxy ~class-and-interfaces ~constructor-args)]
     (update-proxy pc# ~method-name-fn-map);(into {} (map (fn [[k# v#]] [(.intern (str k#)) v#]) ~method-name-fn-map)))
     pc#))

(defn ifn-arity
  "Return a set of numbers #{(0..19)*} or #{(0..19)* :rest} denoting valid arities/varargs
  of a clojure Function. Throws an IllegalArgumentException if (ifn? f) fails."
  [f]
  (when-not (ifn? f) (throw-arg "Expecting an IFn, got a: %s" (type f)))
  (pipe (seq (.getDeclaredMethods (class f)))
        (filter #(#{"doInvoke" "invoke"} (.getName %)))
        (map #(if (= (.getName %) "doInvoke")
                :rest
                (count (.getParameterTypes %))))
        (set)))

;;     #^{:test (and (let [x (fn-arity (fn ([] 'zero-args) ([a b] :two-args)))]
;;                     (or (= x '[(2 0), nil])
;;                         (= x '[(0 2), nil])))
;;                   (= (fn-arity (fn [])) '[(0),nil])
;;                   (= (fn-arity (fn [& rest])) '[nil,true]))}

(def fn-arg-count ifn-arity)

(defn get-methods-by-name
  "get methods from class matching (str name)."
  [class method-name]
  (filter #(= (str method-name) (.getName %)) (seq (.getMethods class))))
;; ex: (first (get-methods-by-name ButtonPressListener 'buttonPressed))))

(defn list-classpaths
  "Return a list of classpaths from the System/getProperty java.class.path."
  []
  (seq (.split (System/getProperty "java.class.path") (System/getProperty "path.separator"))))

(defn list-bootclasspaths
  "Return the bootclasspaths; sun-jvm only."
  []
  (if-let [bcp (System/getProperty "sun.boot.class.path")]
    (seq (.split bcp (System/getProperty "path.separator")))))

;;; ---------------
;;; unordered stuff
;;; ---------------

;; hashmap with custom eq fn?????

(defn unordered-pair= [[a1 b1] [a2 b2]]
  (or (and (= a1 a2) (= b1 b2))
      (and (= a1 b2) (= b1 a2))))

;; derived from boot.clj
;(defn my-distinct [eq-fn coll]
;  (let [step (fn step [[f & r :as xs] seen]
;                 (when xs
;                   (if (some (partial eq-fn f) seen) (recur r seen)
;                       (lazy-cons f (step r (conj seen f))))))]
;    (step (seq coll) nil)))
; ex.: (my-distinct unordered-pair= '([a b] [b a] [c d] [a b] [c d] [g f] [d c]))



;; from: cgrand: http://clj-me.blogspot.com/2008/05/jumping-to-javadocs-from-repl.html
(defn open-url [url]
  (let [htmlpane (new javax.swing.JEditorPane url)]
    (.setEditable htmlpane false)
    (.addHyperlinkListener htmlpane
      (proxy [javax.swing.event.HyperlinkListener] []
        (hyperlinkUpdate [#^javax.swing.event.HyperlinkEvent e]
          (when (= (.getEventType e) (. javax.swing.event.HyperlinkEvent$EventType ACTIVATED))
            (if (instance? javax.swing.text.html.HTMLFrameHyperlinkEvent e)
              (.. htmlpane getDocument (processHTMLFrameHyperlinkEvent e))
              (try
                (.setPage htmlpane (.getURL e))
                (catch Throwable t
                               (.printStackTrace t))))))))
    (doto (new javax.swing.JFrame)
      (.setContentPane (new javax.swing.JScrollPane htmlpane))
      (.setBounds 32 32 700 900)
      (.show))))


(def javadoc-root-url "file:///home/timmy-turner/doc/java-sdk-docs-1.6.0/html/api/")
;;(def javadoc-root-url "http://java.sun.com/javase/6/docs/api/")

(defn javadoc [c]
  "ex: (javadoc Throwable) opens a window displaying Throwable's javadoc.
  Uses javadoc-root-url as the source for javadocs."
  (let [url (str javadoc-root-url
                 (.. c getName (replace \. \/) (replace \$ \.)) ".html")]
    (open-url url)))

(defn ->helpme []
  '((-> people fields count) = (count (fields people))))


(defn atoms-refs-agents []
  "
                                     
               +---------+---------+
   independent | ATOMS   |  AGENTS |
               +---------+---------+
     dependent | REFS    |         |
               +---------+---------+
                 sync       async   
")                                  
                               
;; http://paste.lisp.org/display/71574 
; (defn memoize [f]
;   (let [mem (atom {})]
;     (fn [& args]
;       (if-let [e (find @mem args)]
;         (val e)
;         (let [ret (apply f args)]
;           (swap! mem assoc args ret)
;           ret)))))

; (defn fib [n]
;   (if (<= n 1)
;     n
;     (+ (fib (dec n)) (fib (- n 2)))))



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
