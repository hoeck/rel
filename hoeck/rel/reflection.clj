
;; example relations: reflection

(ns hoeck.rel.reflection
  (:use hoeck.rel
        hoeck.library)
  (:import (java.io File, FileInputStream)
           (java.util.jar JarInputStream, JarEntry)))

(let [primitive-types {'boolean Boolean/TYPE
                       'int     Integer/TYPE
                       'float   Float/TYPE
                       'double  Double/TYPE
                       'byte    Byte/TYPE
                       'long    Long/TYPE
                       'char    Character/TYPE
                       'short   Short/TYPE
                       'void    Void/TYPE}] ;; or use nil for void??
  (defn sym->class 
    "Converts a symbol or string to a java.lang.Class. Works for primitive types too.
  Returns nil if class nymed by s doesn't exist.."
    [s]        
    (or (primitive-types (symbol s))
        (try (Class/forName (str s))
             (catch Throwable e nil)))))

(defn class->sym
  "Converts a Class to a Symbol. For arrays, returns the component name."
  [c]
  (if (.isArray c)
    (class->sym (.getComponentType c))
    (symbol (.getName c))))

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
    "Returns a seq of modifiers from a class/method/field/constructor (reflection) object."
    ([] modif)
    ([id] (map #(-> (group-by modif :id) (get %) first :key)
               (filter #(not= 0 (bit-and id %)) (map #(bit-shift-left 1 %) (range 12)))))))


;; Relation ctors:

;; namespaces

(defn make-namespace-R
  "Returns a relation containing all namespaces in ns-seq."
  [ns-seq]
  (make-relation 
   (map #(list (ns-name %)) ns-seq)
   :fields [:name]))

(defn make-from-namespace-R
  "takes the relation created with `namespace-R' and a clojure.core/ns-* function and
  a field name to where the function maps and produces a new relation."
  [namespace-rel ns-fn field-name]
  (let [ns-names (field-seq namespace-rel :name)
        fields (list :ns :name field-name)]
    (make-relation (mapcat (fn [ns-name] 
                             (map #(zipmap fields
                                           (cons ns-name %))
                                  (ns-fn (find-ns ns-name))))
                           ns-names)
                   :fields fields)))

(defn make-ns-imports-R [ns-relation]
  (make-from-namespace-R ns-relation
                         #(map (fn [[k v]] [k (class->sym v)]) (ns-imports %))
                         :class))

(def make-ns-aliases-R #(make-from-namespace-R % ns-aliases :alias))
(def make-ns-refers-R #(make-from-namespace-R % ns-refers :varname))
(def make-ns-interns-R #(make-from-namespace-R % ns-interns :varname))
(def make-ns-publics-R #(make-from-namespace-R % ns-publics :varname))

;; files

(defn make-file-R
  "Given one or more paths, returns a relation of all files below this path
  using `clojure.core/file-seq'."
  [path-seq]
  (make-relation (map (fn [f] {:name (.getName f)
                               :path (.getParent f)
                               :time (.lastModified f)
                               :directory (.isDirectory f)
                               :size (.length f)})
                      (mapcat #(file-seq (java.io.File. %)) path-seq))))

(defn- read-files-from-jar
  [pathname filename]
  (def _xxx [pathname, filename])
  (let [inp (JarInputStream. (FileInputStream. (str pathname File/separator filename)))
        files (doall (take-while identity (repeatedly #(.getNextJarEntry inp))))]
    (.close inp)
    (map (fn [je] ;; java.util.jar.jarEntry
           {:path pathname
            :jar filename
            :name (.getName je)
            :size (.getSize je)
            :compressed-size (.getCompressedSize je)
            :time (.getTime je)
            :comment (.getComment je)
            :directory (.isDirectory je)})
         files)))

(defn make-jar-R
  "Given a file-R, return a relation of all entries from the jars
  of the file-R."
  [file-R]
  (make-relation (mapcat #(read-files-from-jar (:path %) (:name %))
                         (select file-R (rlike ~name ".*\\.jar$")))))

(defn- path->package
  ;; may return more than one possible packagename (if one classpath contains another)
  [classpaths path]
  (if (and path (string? path))
    (let [possible-classpaths (filter #(.startsWith path %) classpaths)
          extract-packagename (fn [cp] (.replace (let [s (.substring path (count cp))]
                                                   (if (.startsWith s File/separator)
                                                     (.substring s 1 (count s))
                                                     s))
                                                 File/separatorChar 
                                                 \.))]
      (map extract-packagename possible-classpaths))))

(defn- without-dotclass
  "removes the trailing \".class\" from a string."
  [s]
  (and s (string? s) (.substring s 0 (- (count s) 6))))

(defn- classnames-from-files
  "Given a file relation, "
  [file-relation]
  (let [classfiles (select file-relation (rlike ~name ".*\\.class$"))
        cp (list-classpaths)]
    (pipe (project classfiles :path :name)
          (mapcat (fn [tup]
                    (map #(str % "." (without-dotclass (:name tup)))
                         (path->package cp (:path tup)))))
          (filter sym->class))))

;; assume classpath inside the jar to be .; (ignore the MANIFESTs classpath-line)
(defn- classnames-from-jars
  [jar-R]
  (let [cp (list-classpaths)]
    (set (map (fn [tup] (symbol (.replace (without-dotclass (:name tup)) \/ \.)))
              (select jar-R (and (not ~directory)
                                 (rlike ~name ".*\\.class$")))))))

;; classes

(defn- class-tuple
  "Creates a tuple from a java.lang.Class object."
  [c]
  {:name (class->sym c)
   :type (cond (.isEnum c) :enum
               (.isInterface c) :interface
               (.isAnonymousClass c) :anonymous
               (.isLocalClass c) :local
               (.isPrimitive c) :primitive
               (.isSynthetic c) :synthetic
               :else :class)
   :super (if-let [n (.getSuperclass c)]
            (class->sym n))})

(defn- classnames-from-ns
  "Given the ns-imports relation, return all mentioned classes."
  [ns-imports-R]
  (let [classes (map sym->class (field-seq ns-imports-R :class))]
    (reduce conj  
            #{}
            (concat classes (mapcat supers classes)))))

(defn- find-classnames
  [ns-imports-R, file-R, jar-R]
  (clojure.set/union (classnames-from-ns ns-imports-R)
                     (classnames-from-files file-R)
                     (classnames-from-jars jar-R)))

(defn make-class-R [class-seq]
   (make-relation (reduce conj
                          #{}
                          (mapcat class-tuple (map sym->class class-seq)))))

(defn- class-interfaces [c]
  (map (fn [i] {:interface (symbol (.getName i))
                :class (symbol (.getName c))})
       (seq (.getInterfaces c))))

(defn make-interface-R [class-relation]
  (make-relation (set (mapcat #(-> (sym->class %) class-interfaces) (field-seq class-relation :name)))))

(defn- class-methods [class-symbol]
  (map (fn [m] {:class class-symbol
                :declaring-class (symbol (.getName (.getDeclaringClass m)))
                :name (symbol (.getName m))
                :arity (count (.getParameterTypes m))
                :returntype (class->sym (.getReturnType m))
                :returns-array (.isArray (.getReturnType m))})
       (seq (.getMethods (sym->class class-symbol)))))

(defn make-method-R [class-relation]
  (make-relation (set (mapcat class-methods (field-seq class-relation :name)))))

(defn method-arguments [class-symbol]
  (mapcat (fn [m] (map (fn [c p] {:class class-symbol
                                  :method (symbol (.getName m))
                                  :type (symbol (.getName c))
                                  :position p})
                       (.getParameterTypes m)
                       (range (count (.getParameterTypes m)))))
          (.getDeclaredMethods (sym->class class-symbol))))

(defn make-method-arguments-R [method-relation]
  (make-relation (set (mapcat method-arguments (field-seq method-relation :class)))))


(defn- modifier-seq [name, mod-id]
  (map (fn [m] {:name name
                :modifier m})
       (modifiers mod-id)))

(defn make-class-modifier-R [class-rel]
  (make-relation (mapcat (fn [c] (modifier-seq (:name c) (.getModifiers c)))
                         (project class-rel [(sym->class ~name) :name]))))

(defn make-method-modifier-R [method-rel]
   (make-relation (mapcat (fn [tup] (map (fn [m] (map #(assoc %
                                                         :class
                                                         (:class tup))
                                                      (modifier-seq (symbol (.getName m)) (.getModifiers m))))
                                         (.getMethods (:the-class tup))))
                          (project method-rel
                                   :class
                                   [(sym->class ~name) :the-class]))))

(defn find-more-classes 
  "Look in all reflection relations for not-yet seen classses,
  eg. returnvalues from methods, method-args, interfaces."
  [relation-map]
  (binding [*relations* relation-map]
    (field-seq (let [classes (rename (project :classes :name) :name :class)]
                 (difference (union (rename (project :implements :interface) :interface :class)
                                    (rename (project :imports :import) :import :class)
                                    (rename (project :method-args :type) :type :class)
                                    (rename (project :methods :returntype) :returntype :class))
                             classes))
                :class)))


(defn build-reflection-relations 
  ([& opts]
     (let [opts (as-keyargs opts {:files (concat (list-classpaths) (list-bootclasspaths))
                                  :filename-filter-regex nil
                                  :namespaces (all-ns)})

           ;; namespace relations
           ns-rel (make-namespace-R (:namespaces opts))
           imports-rel (make-ns-imports-R ns-rel)
           aliases-rel (make-ns-aliases-R ns-rel)
           refers-rel (make-ns-refers-R ns-rel)
           interns-rel (make-ns-interns-R ns-rel)
           publics-rel (make-ns-publics-R ns-rel)
           
           ;; files
           file-rel (let [fr (make-file-R (:files opts))]
                      (if-let [rx (:filename-filter-regex opts)]
                        (select fr (rlike ~name rx))
                        fr))
           jar-rel (make-jar-R file-rel)
           
           ;; java reflection
           classnames (find-classnames imports-rel file-rel jar-rel)
           class-rel (make-class-R classnames)
           method-rel (make-method-R class-rel)
           method-args-rel (make-method-arguments-R method-rel)
           interfaces-rel (make-interface-R class-rel)
           class-modifiers-rel (make-class-modifier-R class-rel)
           method-modifiers-rel (make-method-modifier-R method-rel)]
       

       {:namespace ns-rel
        :imports imports-rel
        :aliases aliases-rel
        :refers refers-rel
        :interns interns-rel
        :publics publics-rel
        
        :files file-rel
        :jars jar-rel

        :classes class-rel
        :interfaces interfaces-rel
        :method-rel method-rel
        :method-args method-args-rel
        :class-modifiers class-modifiers-rel
        :method-modifiers method-modifiers-rel})))

(defmacro with-relations [& body]
  `(binding [*relations* (merge *relations* (build-reflection-relations))]
     ~@body))

(defn missing-classes? [relations]
  (< (count (:classes relations))
     (count (find-more-classes relations))))

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


(missing-classes? (build-reflection-relations (find-more-classes (build-reflection-relations (find-more-classes (build-reflection-relations (find-more-classes (build-reflection-relations (find-more-classes (build-reflection-relations))))))))))

)




