




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


(in-ns 'hoeck.rel)

(hoeck.rel.reflection/with-relations (def *relations* *relations*))
(select :classes (like ~name 'APersistentSet))


(in-ns 'hoeck.rel.structmaps)

(run-tests)

(in-ns 'user)
(require '[hoeck.rel.reflection :as rf])

(def rels (rf/with-relations *relations*))
(rf/with-relations
 (order-by (relations) :name '<))
#{{:field :name,            :name :aliases    }
  {:field :alias,           :name :aliases    }
  {:field :ns-name,         :name :aliases    }
  {:field :name,            :name :classes    }
  {:field :type,            :name :classes    }
  {:field :super,           :name :classes    }
  {:field :name,            :name :files      }
  {:field :size,            :name :files      }
  {:field :path,            :name :files      }
  {:field :class,           :name :implements }
  {:field :interface,       :name :implements }
  {:field :import,          :name :imports    }
  {:field :name,            :name :imports    }
  {:field :ns-name,         :name :imports    }
  {:field :varname,         :name :interns    }
  {:field :ns-name,         :name :interns    }
  {:field :name,            :name :interns    }
  {:field :class,           :name :method-args}
  {:field :method,          :name :method-args}
  {:field :position,        :name :method-args}
  {:field :type    ,        :name :method-args}
  {:field :class,           :name :methods    }
  {:field :name,            :name :methods    }
  {:field :returntype,      :name :methods    }
  {:field :declaring-class, :name :methods    }
  {:field :arity,           :name :methods    }
  {:field :name,            :name :modifiers  }
  {:field :modifier,        :name :modifiers  }
  {:field :namespace,       :name :namespaces }
  {:field :name,            :name :namespaces }
  {:field :varname,         :name :publics    }
  {:field :ns-name,         :name :publics    }
  {:field :name,            :name :publics    }
  {:field :name,            :name :refers     }
  {:field :varname,         :name :refers     }
  {:field :ns-name,         :name :refers     }}





(count (find-more-classes rels))
(count (:classes rels))
       

(binding [*relations* rels]
  (select :methods (and (like ~class 'BasicFactory)
                        (like ~name 'Predicate)))
  (join (select :methods (like ~class 'IPredicate)) :name
        :method-args :method)

  (select :methods (like ~:class 'IPredicate)))


#{{:name org.deri.iris.api.basics.IPredicate, :type :interface, :super nil}}




#{{:class org.deri.iris.basics.BasicFactory,       :method createPredicate, :argument java.lang.String, :position 0}
  {:class org.deri.iris.api.factory.IBasicFactory, :method createPredicate, :argument int,              :position 1}
  {:class org.deri.iris.api.factory.IBasicFactory, :method createPredicate, :argument java.lang.String, :position 0}
  {:class org.deri.iris.basics.BasicFactory,       :method createPredicate, :argument int,              :position 1}}


(map #(.getName %) (.getMethods org.deri.iris.api.basics.IPredicate))
("getPredicateSymbol" "getArity" "compareTo")
(map #(.getName %) (.getDeclaredMethods org.deri.iris.api.basics.IPredicate))
("getPredicateSymbol" "getArity")




#{{:class org.deri.iris.api.basics.IPredicate, :declaring-class java.lang.Comparable, :name compareTo, :arity 1, :returntype int, :argument java.lang.Float,                     :position 0}
  {:class org.deri.iris.api.basics.IPredicate, :declaring-class java.lang.Comparable, :name compareTo, :arity 1, :returntype int, :argument java.lang.Object,                    :position 0}
  {:class org.deri.iris.api.basics.IPredicate, :declaring-class java.lang.Comparable, :name compareTo, :arity 1, :returntype int, :argument org.deri.iris.api.basics.ITuple,     :position 0}
  {:class org.deri.iris.api.basics.IPredicate, :declaring-class java.lang.Comparable, :name compareTo, :arity 1, :returntype int, :argument java.lang.Byte,                      :position 0}
  {:class org.deri.iris.api.basics.IPredicate, :declaring-class java.lang.Comparable, :name compareTo, :arity 1, :returntype int, :argument java.lang.Short,                     :position 0}
  {:class org.deri.iris.api.basics.IPredicate, :declaring-class java.lang.Comparable, :name compareTo, :arity 1, :returntype int, :argument java.math.BigDecimal,                :position 0}
  {:class org.deri.iris.api.basics.IPredicate, :declaring-class java.lang.Comparable, :name compareTo, :arity 1, :returntype int, :argument java.lang.Double,                    :position 0}
  {:class org.deri.iris.api.basics.IPredicate, :declaring-class java.lang.Comparable, :name compareTo, :arity 1, :returntype int, :argument java.io.File,                        :position 0}
  {:class org.deri.iris.api.basics.IPredicate, :declaring-class java.lang.Comparable, :name compareTo, :arity 1, :returntype int, :argument java.math.BigInteger,                :position 0}
  {:class org.deri.iris.api.basics.IPredicate, :declaring-class java.lang.Comparable, :name compareTo, :arity 1, :returntype int, :argument java.lang.Long,                      :position 0}
  {:class org.deri.iris.api.basics.IPredicate, :declaring-class java.lang.Comparable, :name compareTo, :arity 1, :returntype int, :argument java.lang.Boolean,                   :position 0}
  {:class org.deri.iris.api.basics.IPredicate, :declaring-class java.lang.Comparable, :name compareTo, :arity 1, :returntype int, :argument java.lang.Character,                 :position 0}
  {:class org.deri.iris.api.basics.IPredicate, :declaring-class java.lang.Comparable, :name compareTo, :arity 1, :returntype int, :argument java.lang.Enum,                      :position 0}
  {:class org.deri.iris.api.basics.IPredicate, :declaring-class java.lang.Comparable, :name compareTo, :arity 1, :returntype int, :argument java.lang.Integer,                   :position 0}
  {:class org.deri.iris.api.basics.IPredicate, :declaring-class java.lang.Comparable, :name compareTo, :arity 1, :returntype int, :argument java.lang.String,                    :position 0}
  {:class org.deri.iris.api.basics.IPredicate, :declaring-class java.lang.Comparable, :name compareTo, :arity 1, :returntype int, :argument org.deri.iris.api.basics.IPredicate, :position 0}}

#{{:class org.deri.iris.api.basics.IPredicate, :declaring-class org.deri.iris.api.basics.IPredicate, :name getPredicateSymbol, :arity 0, :returntype java.lang.String}
  {:class org.deri.iris.api.basics.IPredicate, :declaring-class java.lang.Comparable,                :name compareTo,          :arity 1, :returntype int             }
  {:class org.deri.iris.api.basics.IPredicate, :declaring-class org.deri.iris.api.basics.IPredicate, :name getArity,           :arity 0, :returntype int             }}

#{{:class org.deri.iris.basics.BasicFactory,       :declaring-class org.deri.iris.basics.BasicFactory,       :name createPredicate, :arity 2, :returntype org.deri.iris.api.basics.IPredicate}
  {:class org.deri.iris.api.factory.IBasicFactory, :declaring-class org.deri.iris.api.factory.IBasicFactory, :name createPredicate, :arity 2, :returntype org.deri.iris.api.basics.IPredicate}}






