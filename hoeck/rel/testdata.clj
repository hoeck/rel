
(ns hoeck.rel.testdata
  (:use hoeck.rel.operators))

(def people
     '#{{:id 1 :name weilandt,     :vorname mathias,   :adress-id 100}
        {:id 2 :name kirsch,       :vorname diana      :adress-id 100}
        {:id 3 :name schmock,      :vorname robert     :adress-id 101}
        {:id 4 :name hamann,       :vorname robert     :adress-id 102}
        {:id 5 :name soehnel,      :vorname erik       :adress-id 103}
        {:id 6 :name zschieschang, :vorname mandy      :adress-id 103}
        {:id 7 :name unknown,      :vorname unknown    :adress-id 104}})

(def address
     '#{{:id 100 :city dresden :street fickpieschener-allee}
        {:id 101 :city dresden :street plattenbauweg}
        {:id 102 :city rostock :street plattenbauweg}
        {:id 103 :city dresden :street bighaynstr}})

(def agg-test-rel 
     #{{:group :a :age 1 :power 1}
       {:group :a :age 2 :power 2}
       {:group :b :age 1 :power 100}
       {:group :c :age 2 :power 1}
       {:group :c :age 2 :power 2}
       {:group :d :age 10 :power 3}
       {:group :d :age 10 :power 10}
       {:group :d :age 10 :power 10}})