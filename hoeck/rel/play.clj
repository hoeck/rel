


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

(outer-join people :id (rename (project (select people (condition (< ~id 3))) '(:id :name)) {:id :id2, :name :name2}) :id2)
#{{:id 7, :name unknown, :vorname unknown, :adress-id 104}
  {:id2 2, :name2 kirsch, :id 2, :name kirsch, :vorname diana, :adress-id 100}
  {:id 6, :name zschieschang, :vorname mandy, :adress-id 103}
  {:id 5, :name soehnel, :vorname erik, :adress-id 103}
  {:id 4, :name hamann, :vorname robert, :adress-id 102}
  {:id 3, :name schmock, :vorname robert, :adress-id 101}
  {:id2 1, :name2 weilandt, :id 1, :name weilandt, :vorname mathias, :adress-id 100}}

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





