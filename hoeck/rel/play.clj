
;; repl setup
(do
  (in-ns 'user)
  (require 'hoeck.rel.structmaps)
  (require 'hoeck.magic-map.MagicMap :reload)
  (require 'hoeck.value-mapped-map.ValueMappedMap :reload)
  (use 'clojure.contrib.test-is))

(ns hoeck.rel.structmaps)

(use 'de.kotka.lazymap)
(use 'hoeck.value-mapped-map)

(def xx (lazy-hash-map :a (do (println "foo") :foo)))
(def mm (value-mapped-map #(keyword (str %1 "-mapped")) xx))

(lazy-assoc* mm :b (delay :c))
(.lazyAssoc mm :b (delay :c))


(.empty mm)

(hoeck.value_mapped_map.ValueMappedMap. #<core$identity__3551 clojure.core$identity__3551@1292ba7> {} nil)

(type (lazily-rename-keys mm {:a :aa :b :bb :c :cc}))
hoeck.value_mapped_map.ValueMappedMap
hoeck.value_mapped_map.ValueMappedMap
{:aa ::foo-mapped}

(index (rename (select (project people '(:id :name)) (condition (< ~id 4))) {:id :ident-number}))
(index (select (project people '(:id :name)) (condition (< ~id 4))))
{:name {weilandt #{{:name weilandt, :id 1}}, schmock #{{:name schmock, :id 3}}, hamann #{}, soehnel #{}, zschieschang #{}, kirsch #{{:name kirsch, :id 2}}, unknown #{}}
 :id {1 #{{:name weilandt, :id 1}}, 3 #{{:name schmock, :id 3}}, 4 #{}, 5 #{}, 6 #{}, 2 #{{:name kirsch, :id 2}}, 7 #{}}}

(def yyy (select (project people '(:id :name)) (condition (< ~id 4))))
(index yyy)
{:name {weilandt #{{:name weilandt, :id 1}}, schmock #{{:name schmock, :id 3}}, hamann #{}, soehnel #{}, zschieschang #{}, kirsch #{{:name kirsch, :id 2}}, unknown #{}}, :id {1 #{{:name weilandt, :id 1}}, 3 #{{:name schmock, :id 3}}, 4 #{}, 5 #{}, 6 #{}, 2 #{{:name kirsch, :id 2}}, 7 #{}}}
#{{:name kirsch, :id 2} {:name schmock, :id 3} {:name weilandt, :id 1}}
(def xxx (rename yyy {:id :ident-number}))

(def A (project (select people (condition (or (= 'robert ~vorname) (= ~id 1)))) '(:name :vorname)))
(def B (project (select people (condition (< 1 ~id 4))) '(:name :vorname)))

(index (union A B))
(index (difference A B))
(index (intersection A B))

A -> #{{:vorname robert, :name hamann} {:vorname robert, :name schmock} {:vorname mathias, :name weilandt}}
B -> #{{:vorname diana, :name kirsch} {:vorname robert, :name schmock}} 
(intersection A B) -> #{{:vorname robert, :name schmock}}
(difference A B) -> #{{:vorname robert, :name hamann} {:vorname mathias, :name weilandt}}
(difference B A) -> #{{:vorname diana, :name kirsch}}
(= (union A B) (union B A)) -> true
(= (intersection A B) (intersection B A)) -> true

(let [sec {:a 1 :c 3}]
  (magic-map {:a 1 :b 2} (fn ([] (keys sec))
                           ([k] (sec k))))) 

(use 'clojure.contrib.test-is)
(run-tests 'hoeck.rel.structmaps)

(let [i (index (project-expression (make-relation testdata/people) (condition (str ~vorname "-" ~id))))]
  (.entryAt i :name))

(use 'clojure.contrib.pprint)

(with-testdata
  (let [j (join R :adress-id S :id)]
    (map #(= (% (index j))
             (% (make-index j (fields j))))
         (fields j))
;    (fields j)
;    [(:city (index j))

  (let [R (make-relation #{{:a 1 :b 1} {:a 2 :b 1}})
        S (make-relation #{{:a 3 :b 1}})
        T (make-relation #{{:a 1 :b 1}})        
        u (union R S)
        d (difference R T)]
    (difference T R))



(in-ns 'hoeck.rel)

(clojure.contrib.test-is/run-tests 'hoeck.rel.structmaps)

(determine-column-sizes (project* people (fields people)) *pretty-print-relation-opts*)
{:adress-id 14, :vorname 16, :name 18, :id 5}

(def ttt (make-relation
#{{:a 1 :b 2 :c 1}
  {:a 1 :b 1 :c 3}
  {:a 2 :b 2 :c 3}}))
(def ittt (index ttt))


{:a {2 #{{:a 2, :b 2}}, 1 #{{:a 1, :b 1} {:a 1, :b 2}}}, :b {2 #{{:a 1, :b 2} {:a 2, :b 2}}, 1 #{{:a 1, :b 1}}}}


(get-in (:a (index ttt)) [2])
#{{:a 2, :b 2}}
#{{:a 2, :b 2}}

(group-by ttt :b)
{2 #{{:a 1, :b 2} {:a 2, :b 2}}, 1 #{{:a 1, :b 1}}}
{2 {2 #{{:a 2, :b 2}}}, 1 {1 #{{:a 1, :b 1}, 2 {:a 1, :b 2}}}}


(keys (group-by ttt :a :b))

(distinct (map :b (apply concat (vals (:a (index ttt))))))
(2 1)

({:a 2, :b 2} {:a 1, :b 1} {:a 1, :b 2})
{2 #{{:a 2, :b 2}}, 1 #{{:a 1, :b 1} {:a 1, :b 2}}}


(require 'clojure.contrib.pprint)
ittt
(clojure.contrib.pprint/pprint '{:a {2 #{{:a 2, :b 2, :c 3}}, 1 #{{:a 1, :b 2, :c 1} {:a 1, :b 1, :c 3}}}, :c {3 #{{:a 2, :b 2, :c 3} {:a 1, :b 1, :c 3}}, 1 #{{:a 1, :b 2, :c 1}}}, :b {1 #{{:a 1, :b 1, :c 3}}, 2 #{{:a 1, :b 2, :c 1} {:a 2, :b 2, :c 3}}}})
{:a
 {2 #{{:c 3, :b 2, :a 2}}, 1 #{{:c 1, :b 2, :a 1} {:c 3, :b 1, :a 1}}},
 :c
 {3 #{{:c 3, :b 2, :a 2} {:c 3, :b 1, :a 1}}, 1 #{{:c 1, :b 2, :a 1}}},
 :b
 {1 #{{:c 3, :b 1, :a 1}}, 2 #{{:c 1, :b 2, :a 1} {:c 3, :b 2, :a 2}}}}

(ittt :a)
{2 #{{:a 2, :b 2, :c 3}}, 1 #{{:a 1, :b 2, :c 1} {:a 1, :b 1, :c 3}}}
(ittt :b)
{1 #{{:a 1, :b 1, :c 3}}, 2 #{{:a 1, :b 2, :c 1} {:a 2, :b 2, :c 3}}}
(ittt :c)
{3 #{{:a 2, :b 2, :c 3} {:a 1, :b 1, :c 3}}, 1 #{{:a 1, :b 2, :c 1}}}

(defn group-by-lookup [i field-name
  (let [i ittt
      field-names '(:a :b)
      ii (map i field-names)]
  (map (fn [[k v]
            
            (i field-name)))
  
(doc lazy-seq)
-------------------------
clojure.core/lazy-seq
([& body])
Macro
  Takes a body of expressions that returns an ISeq or nil, and yields
  a Seqable object that will invoke the body only the first time seq
  is called, and will cache the result and return it on all subsequent
  seq calls. Any closed over locals will be cleared prior to the tail
  call of body.



{2 #{{:a 2, :b 2, :c 3}}, 1 #{{:a 1, :b 2, :c 1} {:a 1, :b 1, :c 3}}}
{1 #{{:a 1, :b 1, :c 3}}, 2 #{{:a 1, :b 2, :c 1} {:a 2, :b 2, :c 3}}}
#{1 2}
{2 #{{:a 2, :b 2, :c 3}}, 1 #{{:a 1, :b 2, :c 1} {:a 1, :b 1, :c 3}}}
{1 #{{:a 1, :b 1, :c 3}}, 2 #{{:a 1, :b 2, :c 1} {:a 2, :b 2, :c 3}}}
#{nil}





(map #(((index ttt) :b) (:b %)) ((index ttt) :a))




(2 1)

;map:
([f coll]
   (lazy-seq
    (when-let [s (seq coll)]
      (cons (f (first s)) (map f (rest s))))))

(defn extract-fields-from-index [index index-values fields]
  (mapcat #(map (fn [tup] (select-keys tup (conj fields index-key))) (val %)) (index index-key)))

(extract-fields-from-index ittt :a :b :c)
({:b 2, :a 2} {:b 2, :a 1} {:b 1, :a 1})


(defn group-by-keys [i field-names]
  (let [ks (apply extract-fields-from-index i field-names)]

(let [i ittt
      field-names '(:a :b)]

(defn index-part [lookup-fn, tuple-seq, fieldname]
  (magic-map (fn ([] (map fieldname tuple-seq))
                 ([k] (lookup-fn


;;; group-by for 2 fields
(let [i ittt
      field-names '(:a :b)]
  (magic-map (fn ([] (keys (i :a)))
                 ([k1] (magic-map (fn ([] (map :b ((i :a) k1)))
                                      ([k2] (clojure.set/intersection ((i :a) k1) ((i :b) k2)))))))))

(let [i ittt
      field-names '(:a :b)]
  (magic-map (fn ([] (keys (i :a)))
                 ([k] (let [index ((i :a) k)]
                        (magic-map (fn ([] (map :b index))
                                       ([k] (let [index (clojure.set/intersection ((i :b) k))]
                                          (magic-map (fn ([] (map :c index))
                                                         ([k] (let [index (clojure.set/intersection index ((i :c) k))]
                                                                index)))))))))))))


(defn clojure-index
  "Returns an index as from clojure.set/index"
  [R ks]
  (let [gr (apply group-by R ks)]
    (magic-map (fn ([] (tree-seq map? 


(let [gr (group-by ttt :a :b :c)
      f (fn [i cur & more] 
          (map #(apply hash-map cur (key %))
               i))]
  (f gr '(:a :b :c))

(2 1)

(type (first (seq (group-by ttt :a :b :c))))


(doc tree-seq)
-------------------------
clojure.core/tree-seq
([branch? children root])
  Returns a lazy sequence of the nodes in a tree, via a depth-first walk.
   branch? must be a fn of one arg that returns true if passed a node
   that can have children (but may not).  children must be a fn of one
   arg that returns a sequence of the children. Will only be called on
   nodes for which branch? returns true. Root is the root node of the
  tree.
nil

 (doc clojure.set/index)
-------------------------
clojure.set/index
([xrel ks])
  Returns a map of the distinct values of ks in the xrel mapped to a
  set of the maps in xrel with the corresponding values of ks.
nil
(group-by ttt :a :b)
{2 {2 nil}, 1 {2 nil, 1 nil}}

{2 #{{:a 2, :b 2, :c 3}}, 1 #{{:a 1, :b 2, :c 1} {:a 1, :b 1, :c 3}}}

people
#{{:id 7, :name unknown, :vorname unknown, :adress-id 104} {:id 2, :name kirsch, :vorname diana, :adress-id 100} {:id 6, :name zschieschang, :vorname mandy, :adress-id 103} {:id 5, :name soehnel, :vorname erik, :adress-id 103} {:id 4, :name hamann, :vorname robert, :adress-id 102} {:id 3, :name schmock, :vorname robert, :adress-id 101} {:id 1, :name weilandt, :vorname mathias, :adress-id 100}}


(let [i ittt
      field-names '(:a :b)]
  ;;(map :b (vals 
  ((i :a) 2))))

;concat:
(lazy-seq
      (let [s (seq x)]
        (if s
          (cons (first s) (concat (rest s) y))
          y))








(pop (seq '(A v d)))






(defn rpn-parse [#^String s]
  (map (fn [word]
         (if (every? (fn [#^Character c] (Character/isDigit c)) word)
           (Integer/parseInt word)
           @(resolve (symbol word))))
       (.split s "\\s+")))

(defn rpn-fold [tokens]
  (reduce (fn [stack bit]
            (if (#{+ - * /} bit)
              (cons (bit (first stack) (second stack)) (nnext stack))
              (cons bit stack)))
          () tokens))

(def rpn (comp first rpn-fold rpn-parse))


(println (rpn "1 2 3 + - 2 *"))


(let [code (read-string (str "(" "1 2 3 + - 2 *" ")"))]

(defn calc-step
  ([lastresult code]
     (if (seq code)
       (let [[args, rest] (split-with (complement symbol?) code)]
         (calc-step (apply (resolve (first rest)) lastresult args) (next rest)))
       lastresult)))

(calc-step "" (read-string (str "(" "1 *foo*"  ")")))




"a,bc"



(in-ns 'hoeck.rel.structmaps)
(read-string (str "(" "1 2 3 + - 2 * 12 +" ")"))


(let [[r-tup, index-Ss, r, s] r-tupppp]
  (r-tup r)
  (type (val index-Ss)))
hoeck.value_mapped_map.ValueMappedMap

(.state (value-mapped-map identity {:a 1 :b 2}))

false


false


  r-tup)
{:name java.util.concurrent.Callable, :type :interface, :super nil}

(get ((index (make-relation testdata/people)) :name) :asd false)
false
nil
clojure.lang.PersistentArrayMap
nil




{java.lang.StackTraceElement #{{:i-name java.lang.StackTraceElement, :i-type :class, :i-super java.lang.Object}}, clojure.lang.Obj #{{:i-name clojure.lang.Obj, :i-type :class, :i-super java.lang.Object}}, java.lang.StringBuilder #{{:i-name java.lang.StringBuilder, :i-type :class, :i-super java.lang.AbstractStringBuilder}}, clojure.lang.Compiler$CompilerException #{{:i-name clojure.lang.Compiler$CompilerException, :i-type :member, :i-super java.lang.Exception}}, clojure.lang.AMapEntry #{{:i-name clojure.lang.AMapEntry, :i-type :class, :i-super clojure.lang.APersistentVector}}, clojure.lang.Reversible #{{:i-name clojure.lang.Reversible, :i-type :interface, :i-super nil}}, java.lang.Override #{{:i-name java.lang.Override, :i-type :interface, :i-super nil}}, java.io.FilterReader #{{:i-name java.io.FilterReader, :i-type :class, :i-super java.io.Reader}}, java.util.AbstractCollection #{{:i-name java.util.AbstractCollection, :i-type :class, :i-super java.lang.Object}}, java.lang.NoSuchFieldError #{{:i-name java.lang.NoSuchFieldError, :i-type :class, :i-super java.lang.IncompatibleClassChangeError}}, java.lang.IncompatibleClassChangeError #{{:i-name java.lang.IncompatibleClassChangeError, :i-type :class, :i-super java.lang.LinkageError}}, java.util.Map$Entry #{{:i-name java.util.Map$Entry, :i-type :interface, :i-super nil}}, java.lang.InternalError #{{:i-name java.lang.InternalError, :i-type :class, :i-super java.lang.VirtualMachineError}}, hoeck.value_mapped_map.ValueMappedMap #{{:i-name hoeck.value_mapped_map.ValueMappedMap, :i-type :class, :i-super clojure.lang.APersistentMap}}, java.io.IOException #{{:i-name java.io.IOException, :i-type :class, :i-super java.lang.Exception}}, java.lang.UnsatisfiedLinkError #{{:i-name java.lang.UnsatisfiedLinkError, :i-type :class, :i-super java.lang.LinkageError}}, java.lang.AbstractStringBuilder #{{:i-name java.lang.AbstractStringBuilder, :i-type :class, :i-super java.lang.Object}}, java.lang.IllegalAccessException #{{:i-name java.lang.IllegalAccessException, :i-type :class, :i-super java.lang.Exception}}, java.security.Guard #{{:i-name java.security.Guard, :i-type :interface, :i-super nil}}, java.lang.reflect.GenericDeclaration #{{:i-name java.lang.reflect.GenericDeclaration, :i-type :interface, :i-super nil}}, java.lang.reflect.Constructor #{{:i-name java.lang.reflect.Constructor, :i-type :class, :i-super java.lang.reflect.AccessibleObject}}, java.util.AbstractQueue #{{:i-name java.util.AbstractQueue, :i-type :class, :i-super java.util.AbstractCollection}}, clojure.asm.Opcodes #{{:i-name clojure.asm.Opcodes, :i-type :interface, :i-super nil}}, java.util.Set #{{:i-name java.util.Set, :i-type :interface, :i-super nil}}, java.lang.StringIndexOutOfBoundsException #{{:i-name java.lang.StringIndexOutOfBoundsException, :i-type :class, :i-super java.lang.IndexOutOfBoundsException}}, clojure.lang.IPersistentCollection #{{:i-name clojure.lang.IPersistentCollection, :i-type :interface, :i-super nil}}, java.lang.Appendable #{{:i-name java.lang.Appendable, :i-type :interface, :i-super nil}}, java.lang.ArithmeticException #{{:i-name java.lang.ArithmeticException, :i-type :class, :i-super java.lang.RuntimeException}}, org.xml.sax.Attributes #{{:i-name org.xml.sax.Attributes, :i-type :interface, :i-super nil}}, java.util.zip.ZipConstants #{{:i-name java.util.zip.ZipConstants, :i-type :interface, :i-super nil}}, java.lang.SecurityException #{{:i-name java.lang.SecurityException, :i-type :class, :i-super java.lang.RuntimeException}}, java.security.BasicPermission #{{:i-name java.security.BasicPermission, :i-type :class, :i-super java.security.Permission}}, java.io.Serializable #{{:i-name java.io.Serializable, :i-type :interface, :i-super nil}}, java.lang.Runnable #{{:i-name java.lang.Runnable, :i-type :interface, :i-super nil}}, java.lang.Process #{{:i-name java.lang.Process, :i-type :class, :i-super java.lang.Object}}, clojure.lang.IProxy #{{:i-name clojure.lang.IProxy, :i-type :interface, :i-super nil}}, clojure.lang.Streamable #{{:i-name clojure.lang.Streamable, :i-type :interface, :i-super nil}}, clojure.asm.MethodVisitor #{{:i-name clojure.asm.MethodVisitor, :i-type :interface, :i-super nil}}, java.lang.Class #{{:i-name java.lang.Class, :i-type :class, :i-super java.lang.Object}}, java.net.URLClassLoader #{{:i-name java.net.URLClassLoader, :i-type :class, :i-super java.security.SecureClassLoader}}, java.lang.RuntimePermission #{{:i-name java.lang.RuntimePermission, :i-type :class, :i-super java.security.BasicPermission}}, hoeck.rel.Relation #{{:i-name hoeck.rel.Relation, :i-type :class, :i-super clojure.lang.APersistentSet}}, java.lang.Package #{{:i-name java.lang.Package, :i-type :class, :i-super java.lang.Object}}, java.math.BigInteger #{{:i-name java.math.BigInteger, :i-type :class, :i-super java.lang.Number}}, java.io.File #{{:i-name java.io.File, :i-type :class, :i-super java.lang.Object}}, java.lang.Double #{{:i-name java.lang.Double, :i-type :class, :i-super java.lang.Number}}, clojure.lang.Counted #{{:i-name clojure.lang.Counted, :i-type :interface, :i-super nil}}, clojure.lang.IPersistentMap #{{:i-name clojure.lang.IPersistentMap, :i-type :interface, :i-super nil}}, java.lang.Thread$State #{{:i-name java.lang.Thread$State, :i-type :enum, :i-super java.lang.Enum}}, java.lang.RuntimeException #{{:i-name java.lang.RuntimeException, :i-type :class, :i-super java.lang.Exception}}, java.math.BigDecimal #{{:i-name java.math.BigDecimal, :i-type :class, :i-super java.lang.Number}}, java.lang.ThreadGroup #{{:i-name java.lang.ThreadGroup, :i-type :class, :i-super java.lang.Object}}, java.lang.Thread$UncaughtExceptionHandler #{{:i-name java.lang.Thread$UncaughtExceptionHandler, :i-type :interface, :i-super nil}}, clojure.lang.SeqIterator #{{:i-name clojure.lang.SeqIterator, :i-type :class, :i-super java.lang.Object}}, java.io.PushbackReader #{{:i-name java.io.PushbackReader, :i-type :class, :i-super java.io.FilterReader}}, java.lang.InheritableThreadLocal #{{:i-name java.lang.InheritableThreadLocal, :i-type :class, :i-super java.lang.ThreadLocal}}, java.lang.LinkageError #{{:i-name java.lang.LinkageError, :i-type :class, :i-super java.lang.Error}}, java.lang.ThreadLocal #{{:i-name java.lang.ThreadLocal, :i-type :class, :i-super java.lang.Object}}, java.lang.IllegalThreadStateException #{{:i-name java.lang.IllegalThreadStateException, :i-type :class, :i-super java.lang.IllegalArgumentException}}, java.lang.reflect.AnnotatedElement #{{:i-name java.lang.reflect.AnnotatedElement, :i-type :interface, :i-super nil}}, clojure.lang.IMapEntry #{{:i-name clojure.lang.IMapEntry, :i-type :interface, :i-super nil}}, java.lang.reflect.Array #{{:i-name java.lang.reflect.Array, :i-type :class, :i-super java.lang.Object}}, java.lang.Comparable #{{:i-name java.lang.Comparable, :i-type :interface, :i-super nil}}, java.util.Iterator #{{:i-name java.util.Iterator, :i-type :interface, :i-super nil}}, clojure.lang.Sequential #{{:i-name clojure.lang.Sequential, :i-type :interface, :i-super nil}}, java.io.Flushable #{{:i-name java.io.Flushable, :i-type :interface, :i-super nil}}, java.lang.Short #{{:i-name java.lang.Short, :i-type :class, :i-super java.lang.Number}}, hoeck.magic_map.MagicMap #{{:i-name hoeck.magic_map.MagicMap, :i-type :class, :i-super clojure.lang.APersistentMap}}, java.lang.Iterable #{{:i-name java.lang.Iterable, :i-type :interface, :i-super nil}}, java.lang.ClassNotFoundException #{{:i-name java.lang.ClassNotFoundException, :i-type :class, :i-super java.lang.Exception}}, java.lang.NoSuchFieldException #{{:i-name java.lang.NoSuchFieldException, :i-type :class, :i-super java.lang.Exception}}, clojure.lang.Associative #{{:i-name clojure.lang.Associative, :i-type :interface, :i-super nil}}, java.io.FileReader #{{:i-name java.io.FileReader, :i-type :class, :i-super java.io.InputStreamReader}}, java.lang.Math #{{:i-name java.lang.Math, :i-type :class, :i-super java.lang.Object}}, java.security.Permission #{{:i-name java.security.Permission, :i-type :class, :i-super java.lang.Object}}, java.lang.Byte #{{:i-name java.lang.Byte, :i-type :class, :i-super java.lang.Number}}, java.lang.Exception #{{:i-name java.lang.Exception, :i-type :class, :i-super java.lang.Throwable}}, clojure.lang.APersistentMap #{{:i-name clojure.lang.APersistentMap, :i-type :class, :i-super clojure.lang.AFn}}, java.lang.AssertionError #{{:i-name java.lang.AssertionError, :i-type :class, :i-super java.lang.Error}}, java.lang.IllegalArgumentException #{{:i-name java.lang.IllegalArgumentException, :i-type :class, :i-super java.lang.RuntimeException}}, java.lang.reflect.AccessibleObject #{{:i-name java.lang.reflect.AccessibleObject, :i-type :class, :i-super java.lang.Object}}, java.util.RandomAccess #{{:i-name java.util.RandomAccess, :i-type :interface, :i-super nil}}, java.lang.UnsupportedClassVersionError #{{:i-name java.lang.UnsupportedClassVersionError, :i-type :class, :i-super java.lang.ClassFormatError}}, clojure.asm.commons.LocalVariablesSorter #{{:i-name clojure.asm.commons.LocalVariablesSorter, :i-type :class, :i-super clojure.asm.MethodAdapter}}, java.lang.ClassCircularityError #{{:i-name java.lang.ClassCircularityError, :i-type :class, :i-super java.lang.LinkageError}}, java.lang.SecurityManager #{{:i-name java.lang.SecurityManager, :i-type :class, :i-super java.lang.Object}}, java.lang.Readable #{{:i-name java.lang.Readable, :i-type :interface, :i-super nil}}, java.lang.Object #{{:i-name java.lang.Object, :i-type :class, :i-super nil}}, java.util.List #{{:i-name java.util.List, :i-type :interface, :i-super nil}}, java.lang.NegativeArraySizeException #{{:i-name java.lang.NegativeArraySizeException, :i-type :class, :i-super java.lang.RuntimeException}}, java.lang.Thread #{{:i-name java.lang.Thread, :i-type :class, :i-super java.lang.Object}}, java.util.Collection #{{:i-name java.util.Collection, :i-type :interface, :i-super nil}}, clojure.lang.LineNumberingPushbackReader #{{:i-name clojure.lang.LineNumberingPushbackReader, :i-type :class, :i-super java.io.PushbackReader}}, java.io.Reader #{{:i-name java.io.Reader, :i-type :class, :i-super java.lang.Object}}, java.net.SocketException #{{:i-name java.net.SocketException, :i-type :class, :i-super java.io.IOException}}, java.lang.StrictMath #{{:i-name java.lang.StrictMath, :i-type :class, :i-super java.lang.Object}}, clojure.lang.IPersistentSet #{{:i-name clojure.lang.IPersistentSet, :i-type :interface, :i-super nil}}, java.lang.CharSequence #{{:i-name java.lang.CharSequence, :i-type :interface, :i-super nil}}, clojure.lang.MapEntry #{{:i-name clojure.lang.MapEntry, :i-type :class, :i-super clojure.lang.AMapEntry}}, java.lang.IllegalMonitorStateException #{{:i-name java.lang.IllegalMonitorStateException, :i-type :class, :i-super java.lang.RuntimeException}}, org.xml.sax.ContentHandler #{{:i-name org.xml.sax.ContentHandler, :i-type :interface, :i-super nil}}, java.lang.ClassCastException #{{:i-name java.lang.ClassCastException, :i-type :class, :i-super java.lang.RuntimeException}}, java.lang.ThreadDeath #{{:i-name java.lang.ThreadDeath, :i-type :class, :i-super java.lang.Error}}, java.io.Closeable #{{:i-name java.io.Closeable, :i-type :interface, :i-super nil}}, java.lang.NoSuchMethodError #{{:i-name java.lang.NoSuchMethodError, :i-type :class, :i-super java.lang.IncompatibleClassChangeError}}, java.util.concurrent.LinkedBlockingQueue #{{:i-name java.util.concurrent.LinkedBlockingQueue, :i-type :class, :i-super java.util.AbstractQueue}}, java.lang.ClassFormatError #{{:i-name java.lang.ClassFormatError, :i-type :class, :i-super java.lang.LinkageError}}, javax.xml.parsers.SAXParserFactory #{{:i-name javax.xml.parsers.SAXParserFactory, :i-type :class, :i-super java.lang.Object}}, clojure.lang.Reflector #{{:i-name clojure.lang.Reflector, :i-type :class, :i-super java.lang.Object}}, javax.xml.parsers.SAXParser #{{:i-name javax.xml.parsers.SAXParser, :i-type :class, :i-super java.lang.Object}}, clojure.asm.Type #{{:i-name clojure.asm.Type, :i-type :class, :i-super java.lang.Object}}, java.lang.NullPointerException #{{:i-name java.lang.NullPointerException, :i-type :class, :i-super java.lang.RuntimeException}}, java.lang.Error #{{:i-name java.lang.Error, :i-type :class, :i-super java.lang.Throwable}}, clojure.lang.Seqable #{{:i-name clojure.lang.Seqable, :i-type :interface, :i-super nil}}, java.lang.Cloneable #{{:i-name java.lang.Cloneable, :i-type :interface, :i-super nil}}, java.io.Writer #{{:i-name java.io.Writer, :i-type :class, :i-super java.lang.Object}}, java.lang.Float #{{:i-name java.lang.Float, :i-type :class, :i-super java.lang.Number}}, clojure.lang.Compiler #{{:i-name clojure.lang.Compiler, :i-type :class, :i-super java.lang.Object}}, java.util.concurrent.Callable #{{:i-name java.util.concurrent.Callable, :i-type :interface, :i-super nil}}, java.lang.InstantiationError #{{:i-name java.lang.InstantiationError, :i-type :class, :i-super java.lang.IncompatibleClassChangeError}}, java.lang.reflect.Modifier #{{:i-name java.lang.reflect.Modifier, :i-type :class, :i-super java.lang.Object}}, java.lang.ExceptionInInitializerError #{{:i-name java.lang.ExceptionInInitializerError, :i-type :class, :i-super java.lang.LinkageError}}, java.io.InputStreamReader #{{:i-name java.io.InputStreamReader, :i-type :class, :i-super java.io.Reader}}, java.util.zip.ZipFile #{{:i-name java.util.zip.ZipFile, :i-type :class, :i-super java.lang.Object}}, java.lang.IndexOutOfBoundsException #{{:i-name java.lang.IndexOutOfBoundsException, :i-type :class, :i-super java.lang.RuntimeException}}, clojure.lang.IObj #{{:i-name clojure.lang.IObj, :i-type :interface, :i-super nil}}, clojure.asm.commons.Method #{{:i-name clojure.asm.commons.Method, :i-type :class, :i-super java.lang.Object}}, clojure.lang.AFn #{{:i-name clojure.lang.AFn, :i-type :class, :i-super clojure.lang.Obj}}, java.net.ServerSocket #{{:i-name java.net.ServerSocket, :i-type :class, :i-super java.lang.Object}}, java.lang.TypeNotPresentException #{{:i-name java.lang.TypeNotPresentException, :i-type :class, :i-super java.lang.RuntimeException}}, java.security.SecureClassLoader #{{:i-name java.security.SecureClassLoader, :i-type :class, :i-super java.lang.ClassLoader}}, clojure.asm.commons.GeneratorAdapter #{{:i-name clojure.asm.commons.GeneratorAdapter, :i-type :class, :i-super clojure.asm.commons.LocalVariablesSorter}}, java.lang.VirtualMachineError #{{:i-name java.lang.VirtualMachineError, :i-type :class, :i-super java.lang.Error}}, java.util.concurrent.BlockingQueue #{{:i-name java.util.concurrent.BlockingQueue, :i-type :interface, :i-super nil}}, java.lang.reflect.Member #{{:i-name java.lang.reflect.Member, :i-type :interface, :i-super nil}}, java.lang.UnknownError #{{:i-name java.lang.UnknownError, :i-type :class, :i-super java.lang.VirtualMachineError}}, clojure.lang.APersistentVector #{{:i-name clojure.lang.APersistentVector, :i-type :class, :i-super clojure.lang.AFn}}, java.lang.String #{{:i-name java.lang.String, :i-type :class, :i-super java.lang.Object}}, java.lang.CloneNotSupportedException #{{:i-name java.lang.CloneNotSupportedException, :i-type :class, :i-super java.lang.Exception}}, clojure.lang.PersistentHashMap #{{:i-name clojure.lang.PersistentHashMap, :i-type :class, :i-super clojure.lang.APersistentMap}}, java.lang.InterruptedException #{{:i-name java.lang.InterruptedException, :i-type :class, :i-super java.lang.Exception}}, java.lang.InstantiationException #{{:i-name java.lang.InstantiationException, :i-type :class, :i-super java.lang.Exception}}, java.lang.OutOfMemoryError #{{:i-name java.lang.OutOfMemoryError, :i-type :class, :i-super java.lang.VirtualMachineError}}, java.lang.Throwable #{{:i-name java.lang.Throwable, :i-type :class, :i-super java.lang.Object}}, java.lang.SuppressWarnings #{{:i-name java.lang.SuppressWarnings, :i-type :interface, :i-super nil}}, de.kotka.lazymap.LazyMap #{{:i-name de.kotka.lazymap.LazyMap, :i-type :class, :i-super clojure.lang.APersistentMap}}, java.lang.ClassLoader #{{:i-name java.lang.ClassLoader, :i-type :class, :i-super java.lang.Object}}, clojure.lang.IPersistentVector #{{:i-name clojure.lang.IPersistentVector, :i-type :interface, :i-super nil}}, java.lang.Integer #{{:i-name java.lang.Integer, :i-type :class, :i-super java.lang.Number}}, java.lang.ProcessBuilder #{{:i-name java.lang.ProcessBuilder, :i-type :class, :i-super java.lang.Object}}, java.lang.IllegalStateException #{{:i-name java.lang.IllegalStateException, :i-type :class, :i-super java.lang.RuntimeException}}, clojure.lang.APersistentSet #{{:i-name clojure.lang.APersistentSet, :i-type :class, :i-super clojure.lang.AFn}}, org.xml.sax.SAXException #{{:i-name org.xml.sax.SAXException, :i-type :class, :i-super java.lang.Exception}}, java.lang.EnumConstantNotPresentException #{{:i-name java.lang.EnumConstantNotPresentException, :i-type :class, :i-super java.lang.RuntimeException}}, java.lang.System #{{:i-name java.lang.System, :i-type :class, :i-super java.lang.Object}}, clojure.asm.ClassWriter #{{:i-name clojure.asm.ClassWriter, :i-type :class, :i-super java.lang.Object}}, java.lang.UnsupportedOperationException #{{:i-name java.lang.UnsupportedOperationException, :i-type :class, :i-super java.lang.RuntimeException}}, java.lang.Boolean #{{:i-name java.lang.Boolean, :i-type :class, :i-super java.lang.Object}}, java.net.InetAddress #{{:i-name java.net.InetAddress, :i-type :class, :i-super java.lang.Object}}, java.lang.Character #{{:i-name java.lang.Character, :i-type :class, :i-super java.lang.Object}}, java.lang.IllegalAccessError #{{:i-name java.lang.IllegalAccessError, :i-type :class, :i-super java.lang.IncompatibleClassChangeError}}, java.lang.NoClassDefFoundError #{{:i-name java.lang.NoClassDefFoundError, :i-type :class, :i-super java.lang.LinkageError}}, java.lang.Void #{{:i-name java.lang.Void, :i-type :class, :i-super java.lang.Object}}, java.lang.ArrayIndexOutOfBoundsException #{{:i-name java.lang.ArrayIndexOutOfBoundsException, :i-type :class, :i-super java.lang.IndexOutOfBoundsException}}, java.lang.Enum #{{:i-name java.lang.Enum, :i-type :class, :i-super java.lang.Object}}, clojure.lang.DynamicClassLoader #{{:i-name clojure.lang.DynamicClassLoader, :i-type :class, :i-super java.net.URLClassLoader}}, java.lang.VerifyError #{{:i-name java.lang.VerifyError, :i-type :class, :i-super java.lang.LinkageError}}, java.util.Queue #{{:i-name java.util.Queue, :i-type :interface, :i-super nil}}, java.lang.NumberFormatException #{{:i-name java.lang.NumberFormatException, :i-type :class, :i-super java.lang.IllegalArgumentException}}, java.lang.AbstractMethodError #{{:i-name java.lang.AbstractMethodError, :i-type :class, :i-super java.lang.IncompatibleClassChangeError}}, java.util.Map #{{:i-name java.util.Map, :i-type :interface, :i-super nil}}, clojure.asm.MethodAdapter #{{:i-name clojure.asm.MethodAdapter, :i-type :class, :i-super java.lang.Object}}, clojure.lang.IFn #{{:i-name clojure.lang.IFn, :i-type :interface, :i-super nil}}, java.io.OutputStreamWriter #{{:i-name java.io.OutputStreamWriter, :i-type :class, :i-super java.io.Writer}}, java.lang.Number #{{:i-name java.lang.Number, :i-type :class, :i-super java.lang.Object}}, java.lang.annotation.Annotation #{{:i-name java.lang.annotation.Annotation, :i-type :interface, :i-super nil}}, java.lang.Deprecated #{{:i-name java.lang.Deprecated, :i-type :interface, :i-super nil}}, java.io.StringReader #{{:i-name java.io.StringReader, :i-type :class, :i-super java.io.Reader}}, java.net.Socket #{{:i-name java.net.Socket, :i-type :class, :i-super java.lang.Object}}, java.lang.Long #{{:i-name java.lang.Long, :i-type :class, :i-super java.lang.Number}}, clojure.asm.ClassVisitor #{{:i-name clojure.asm.ClassVisitor, :i-type :interface, :i-super nil}}, java.lang.ArrayStoreException #{{:i-name java.lang.ArrayStoreException, :i-type :class, :i-super java.lang.RuntimeException}}, java.io.BufferedReader #{{:i-name java.io.BufferedReader, :i-type :class, :i-super java.io.Reader}}, clojure.lang.IPersistentStack #{{:i-name clojure.lang.IPersistentStack, :i-type :interface, :i-super nil}}, java.lang.Runtime #{{:i-name java.lang.Runtime, :i-type :class, :i-super java.lang.Object}}, java.lang.NoSuchMethodException #{{:i-name java.lang.NoSuchMethodException, :i-type :class, :i-super java.lang.Exception}}, clojure.lang.RT #{{:i-name clojure.lang.RT, :i-type :class, :i-super java.lang.Object}}, java.lang.reflect.Type #{{:i-name java.lang.reflect.Type, :i-type :interface, :i-super nil}}, de.kotka.lazymap.LazyMapEntry #{{:i-name de.kotka.lazymap.LazyMapEntry, :i-type :class, :i-super java.lang.Object}}, clojure.lang.IMeta #{{:i-name clojure.lang.IMeta, :i-type :interface, :i-super nil}}, java.lang.StackOverflowError #{{:i-name java.lang.StackOverflowError, :i-type :class, :i-super java.lang.VirtualMachineError}}, java.lang.StringBuffer #{{:i-name java.lang.StringBuffer, :i-type :class, :i-super java.lang.AbstractStringBuilder}}}

  (let [friend (first ((val index-Ss) (r-tup r)))]
    
    (if friend (merge r-tup (dissoc friend s)))))

(in-ns 'hoeck.rel)

(hoeck.rel.reflection/with-relations (def *relations* *relations*))
(select :classes (like ~name 'APersistentSet))

(fields :methods)
(:class :declaring-class :name :arity :returntype)
(join (select :classes (like ~name 'APersistentSet)) :name
      (as :methods 'm) :m-class)

(fields :methods)
(:class :declaring-class :name :arity :returntype)
(select :methods (like ~class 'APersistentSet))

(let [R (make-relation #{{:a 1}})
      S (make-relation #{{:b 1 :c 1} {:b 1 :c 2}})]
(union (join  R :a
              S :b)
       (join S :b
             R :a)));;; NPE ????????????????
                    

