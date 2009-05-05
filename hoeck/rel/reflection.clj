
;; example relations: reflection

(ns hoeck.rel.reflection
  (:use hoeck.rel
        hoeck.library))

(defn sym->class  [s]
  (Class/forName (name s)))

(defn namespace-R []
  (make-relation 
   (map #(list (ns-name %) %) (all-ns))
   :fields [:name :namespace]))

(defn ns-relation [namespace-rel ns-func field-name]
  (let [fields (list :ns-name :name field-name)]
    (make-relation (mapcat (fn [tup] (map #(zipmap fields (cons (:name tup) %)) 
                                          (ns-func (:namespace tup))))
                           namespace-rel)
                   :fields fields)))

(defn file-relation [path-seq]
  (make-relation (map (fn [f] {:name (.getName f)
                               :path (.getParent f)
                               :size (.length f)})
                      (filter #(not (.isDirectory %)) 
                              (mapcat #(file-seq (java.io.File. %)) path-seq)))
                 :fields [:name :path :size]))

(let [modif (make-relation [[:abstract java.lang.reflect.Modifier/ABSTRACT]
                            [:final java.lang.reflect.Modifier/FINAL]
                            [:interface java.lang.reflect.Modifier/INTERFACE]
                            [:native java.lang.reflect.Modifier/NATIVE]
                            [:private java.lang.reflect.Modifier/PRIVATE]
                            [:protected java.lang.reflect.Modifier/PROTECTED]
                            [:public java.lang.reflect.Modifier/PUBLIC]
                            [:static java.lang.reflect.Modifier/STATIC]
                            [:strict java.lang.reflect.Modifier/STRICT]
                            [:synchronized java.lang.reflect.Modifier/SYNCHRONIZED]
                            [:transient java.lang.reflect.Modifier/TRANSIENT]
                            [:volatile java.lang.reflect.Modifier/VOLATILE]]
                           :fields [:key :id])]
  (defn modifiers 
    ([] modif)
    ([id] (map #(-> (group-by modif :id) (get %) first :key)
               (filter #(not= 0 (bit-and id %)) (map #(bit-shift-left 1 %) (range 12)))))))

(defn class-tuple [c]
  {:name (symbol (.getName c))
   :type (cond (.isInterface c) :interface
               (.isEnum c) :enum
               (.isArray c) :array
               (.isAnonymousClass c) :anonymous
               (.isLocalClass c) :local
               (.isMemberClass c) :member
               (.isPrimitive c) :primitive
               (.isSynthetic c) :synthetic
               :else :class)
   :super (if-let [n (.getSuperclass c)]
            (symbol (.getName n)))})

(defn add-class [classes c]
  (let [new-class (class-tuple c)
        sup (map class-tuple (supers c))]
    (reduce conj classes (conj sup new-class))))

(defn make-classes [imports]
  (make-relation (reduce add-class
                         #{}
                         (field-seq imports :import))))

(defn class-interfaces [c]
  (map (fn [i] {:interface (symbol (.getName i))
                :class (symbol (.getName c))})
       (filter #(.isInterface %) (supers c))))

(defn interfaces [class-relation]
  (make-relation (set (mapcat #(-> (sym->class %) class-interfaces) (field-seq class-relation :name)))))

(defn class-methods [class-symbol]
  (map (fn [m] {:class class-symbol
                :declaring-class (symbol (.getName (.getDeclaringClass m)))
                :name (symbol (.getName m))
                :arity (count (.getParameterTypes m))
                :returntype (symbol (.getName (.getReturnType m)))})
       (seq (.getMethods (sym->class class-symbol)))))

(defn method-relation [class-relation]
  (make-relation (set (mapcat class-methods (field-seq class-relation :name)))))

(defn method-arguments [class-symbol]
  (mapcat (fn [m] (map (fn [c p] {:method (symbol (.getName m))
                                  :argument (symbol (.getName c))
                                  :position p})
                       (.getParameterTypes m)
                       (range (count (.getParameterTypes m)))))
          (.getDeclaredMethods (sym->class class-symbol))))

(defn method-arguments-relation [method-relation]
  (make-relation (set (mapcat method-arguments (field-seq method-relation :class)))))

(defn method-and-class-modifiers [class-relation]
  (let [modifier-seq (fn [name, mod-id] 
                       (map (fn [m] {:name name
                                     :modifier m})
                            (modifiers mod-id)))]
    (make-relation (set (mapcat (fn [c] (concat (mapcat (fn [m] (modifier-seq (symbol (.getName m)) (.getModifiers m)))
                                                        (.getDeclaredMethods c))
                                                (modifier-seq (symbol (.getName c)) (.getModifiers c))))
                                (map sym->class (field-seq class-relation :name)))))))


(defmacro with-relations [& body]
  `(let [namespace-rel# (namespace-R)
         imports# (ns-relation namespace-rel# ns-imports :import)
         classes# (make-classes imports#)
         methods# (method-relation classes#)]
     (binding [*relations* (merge *relations*
                                  {:namespaces namespace-rel#
                                   :aliases (ns-relation namespace-rel# ns-aliases :alias)
                                   :imports imports#
                                   :refers (ns-relation namespace-rel# ns-refers :varname)
                                   :interns (ns-relation namespace-rel# ns-interns :varname)
                                   :publics (ns-relation namespace-rel# ns-publics :varname)
                                   :files (file-relation (list-classpaths))
                                   :classes classes#
                                   :implements (interfaces classes#)
                                   :methods methods#
                                   :method-args (method-arguments-relation methods#)
                                   :modifiers (method-and-class-modifiers classes#)})]
       ~@body)))


(comment 

;; load many libs
(require '(clojure.contrib duck-streams sql command_line cond def duck_streams
                             except fcase import_static javalog lazy_seqs lazy_xml
                             mmap ns_utils seq_utils sql str_utils test_clojure
                             test_is trace zip_filter))

(with-relations
 ;; example: private definitions in the hoeck.rel.structmaps namespace
 (and (= (select (difference :interns :publics) (= ~ns-name 'hoeck.rel.structmaps))
         (get-in (op/index (difference :interns :publics)) [:ns-name 'hoeck.rel.structmaps])) ;; test index
      (select (difference :interns :publics) (= ~ns-name 'hoeck.rel.structmaps))))

;; class relation
(with-relations (*relations* :classes))

;; list all files on the classpath
(with-relations
 (order-by
  (project :files [(/ ~size 1000.0) :size-in-kb] :name :path)
  :size-in-kb
  '<))

;; all classes implementing clojure.lang.IFn
(with-relations (select :implements (like ~interface 'IFn)))

)


