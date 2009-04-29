
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

(get-in (op/index (project people [(= ~id 1) :id-one] :name :id)) [:id-one, false])

(join people :adress-id (op/make-relation td/address) :id)

(empty (index (join (op/make-relation td/address) :id people :adress-id)))

(empty nil)

(empty (index (op/make-relation td/address)))

(= (index (project* people (fields people))) (index people))

(union (union people people) people)


(doc def)
-------------------------
def
Special Form
  Please see http://clojure.org/special_forms#def
nil












(clojure.contrib.test-is/run-tests 'hoeck.rel.structmaps)


Testing hoeck.rel.structmaps

Ran 8 tests containing 42 assertions.
0 failures, 0 errors.
(doc distinct)
-------------------------
clojure.core/distinct
([coll])
  Returns a lazy sequence of the elements of coll with duplicates removed


+(let [xxx (op/xproduct (as people 'a) (as people 'b))]
  (get-in (index xxx) [:a-vorname 'robert]))

(index (make-relation #{{:a 1 :b 1} {:a 1 :b 2} {:b 2}} :fields '(:a :b)))

