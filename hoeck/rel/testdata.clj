
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

(def fields-R '#{:id :name :vorname :adress-id})

(def index-R 
     '{:name {weilandt #{{:id 1, :name weilandt, :vorname mathias, :adress-id 100}}, schmock #{{:id 3, :name schmock, :vorname robert, :adress-id 101}}, hamann #{{:id 4, :name hamann, :vorname robert, :adress-id 102}}, soehnel #{{:id 5, :name soehnel, :vorname erik, :adress-id 103}}, zschieschang #{{:id 6, :name zschieschang, :vorname mandy, :adress-id 103}}, kirsch #{{:id 2, :name kirsch, :vorname diana, :adress-id 100}}, unknown #{{:id 7, :name unknown, :vorname unknown, :adress-id 104}}}
       :vorname {mathias #{{:id 1, :name weilandt, :vorname mathias, :adress-id 100}}, robert #{{:id 4, :name hamann, :vorname robert, :adress-id 102} {:id 3, :name schmock, :vorname robert, :adress-id 101}}, erik #{{:id 5, :name soehnel, :vorname erik, :adress-id 103}}, mandy #{{:id 6, :name zschieschang, :vorname mandy, :adress-id 103}}, diana #{{:id 2, :name kirsch, :vorname diana, :adress-id 100}}, unknown #{{:id 7, :name unknown, :vorname unknown, :adress-id 104}}}
       :adress-id {101 #{{:id 3, :name schmock, :vorname robert, :adress-id 101}}, 102 #{{:id 4, :name hamann, :vorname robert, :adress-id 102}}, 103 #{{:id 6, :name zschieschang, :vorname mandy, :adress-id 103} {:id 5, :name soehnel, :vorname erik, :adress-id 103}}, 100 #{{:id 2, :name kirsch, :vorname diana, :adress-id 100} {:id 1, :name weilandt, :vorname mathias, :adress-id 100}}, 104 #{{:id 7, :name unknown, :vorname unknown, :adress-id 104}}}
       :id {1 #{{:id 1, :name weilandt, :vorname mathias, :adress-id 100}}, 3 #{{:id 3, :name schmock, :vorname robert, :adress-id 101}}, 4 #{{:id 4, :name hamann, :vorname robert, :adress-id 102}}, 5 #{{:id 5, :name soehnel, :vorname erik, :adress-id 103}}, 6 #{{:id 6, :name zschieschang, :vorname mandy, :adress-id 103}}, 2 #{{:id 2, :name kirsch, :vorname diana, :adress-id 100}}, 7 #{{:id 7, :name unknown, :vorname unknown, :adress-id 104}}}})



(def address
     '#{{:id 100 :city dresden :street fickpieschener-allee}
        {:id 101 :city dresden :street plattenbauweg}
        {:id 102 :city rostock :street plattenbauweg}
        {:id 103 :city dresden :street bighaynstr}})

(def fields-S '#{:id :city :street})

(def index-S
     '{:city {rostock #{{:id 102, :city rostock, :street plattenbauweg}}, unknown #{{:id 104, :city unknown, :street unknown}}, dresden #{{:id 101, :city dresden, :street plattenbauweg} {:id 100, :city dresden, :street fickpieschener-allee} {:id 103, :city dresden, :street bighaynstr}}}
       :street {bighaynstr #{{:id 103, :city dresden, :street bighaynstr}}, unknown #{{:id 104, :city unknown, :street unknown}}, fickpieschener-allee #{{:id 100, :city dresden, :street fickpieschener-allee}}, plattenbauweg #{{:id 101, :city dresden, :street plattenbauweg} {:id 102, :city rostock, :street plattenbauweg}}}
       :id {102 #{{:id 102, :city rostock, :street plattenbauweg}}, 103 #{{:id 103, :city dresden, :street bighaynstr}}, 104 #{{:id 104, :city unknown, :street unknown}}, 100 #{{:id 100, :city dresden, :street fickpieschener-allee}}, 101 #{{:id 101, :city dresden, :street plattenbauweg}}}})

(defmacro with-testdata [& body]
  `(let [~'R (make-relation people)
         ~'S (make-relation address)]
     ~@body))

