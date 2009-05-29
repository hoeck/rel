
;; example relations: reflection

(ns hoeck.rel.reflection
  (:use hoeck.rel
        hoeck.library
        hoeck.magic-map)
  (:import (java.io File, FileInputStream)
           (java.util.jar JarInputStream, JarEntry)
           (java.lang.instrument IllegalClassFormatException)))

(defmacro try-ignore
  "Ignore all classloading errors"
  [& body]
  `(try ~@body
        (catch NullPointerException e# nil)
        (catch LinkageError e# nil)
        (catch IllegalClassFormatException e# nil)
        (catch ClassNotFoundException e# nil)))

(let [primitive-types {'boolean Boolean/TYPE
                       'int     Integer/TYPE
                       'float   Float/TYPE
                       'double  Double/TYPE
                       'byte    Byte/TYPE
                       'long    Long/TYPE
                       'char    Character/TYPE
                       'short   Short/TYPE
                       'void    Void/TYPE}] ;; or use nil for void??
  (def sym->class 
;;    "Converts a symbol or string to a java.lang.Class. Works for primitive types too.
;;  Returns nil if class nymed by s doesn't exist.."
       (memoize (fn [s]
                  (and s
                       (or (primitive-types (symbol s))
                           (try-ignore (Class/forName (str s)))))))))

(def class->sym
;;  "Converts a Class to a Symbol. For arrays, returns the component name."
  (memoize (fn [#^Class c]
             (if (.isArray c)
               (class->sym (.getComponentType c))
               (symbol (.getName c))))))

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
    ([id] (map 
           #(-> (group-by modif :id) (get %) first :key)
           (filter #(not= 0 (bit-and id %)) (map #(bit-shift-left 1 %) (range 12)))))))


;; Relation ctors:

(defn make-classpath-R []
  (make-relation (map list (concat (list-classpaths) (list-bootclasspaths)))
                 :fields [:path]))

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
  "Given one or more existing paths, returns a relation of all files below this path
  using `clojure.core/file-seq'."
  [path-seq]
  (make-relation (map (fn [#^File f] {:name (.getName f)
                               :path (.getParent f)
                               :time (.lastModified f)
                               :directory (.isDirectory f)
                               :size (.length f)})
                      
                              (mapcat (fn [#^File f] (file-seq f))
                                      (filter #(.exists #^File %)
                                              (map #(File. #^String %) path-seq))))))

(defn- read-files-from-jar
  [pathname filename]
  (let [inp (JarInputStream. (FileInputStream. (str pathname File/separator filename)))
        files (doall (take-while identity (repeatedly #(.getNextJarEntry inp))))]
    (.close inp)
    (map (fn [#^JarEntry je] ;; java.util.jar.jarEntry
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
                         (select file-R (rlike ~name ".*\\.jar$")))
                 :fields [:path :jar :name :size :compressed-size :time :comment :directory]))

(defn- path->package
  ([#^String path]
     (.replace path File/separatorChar \.))
  ([classpaths #^String path]
     ;; should be a functional join over the classpath-rel
     ;; may return more than one possible packagename (if one classpath contains another)
     (if (and path (string? path))
       (let [possible-classpaths (filter #(.startsWith path %) classpaths)
             extract-packagename (fn [cp] (.replace (let [s (.substring path (count cp))]
                                                      (if (.startsWith s File/separator)
                                                        (.substring s 1 (count s))
                                                        s))
                                                    File/separatorChar 
                                                    \.))]
         (map extract-packagename possible-classpaths)))))

(defn- without-dotclass
  "removes the trailing \".class\" from a string."
  [#^String s]
  (and s (string? s) (.substring s 0 (- (count s) 6))))

;; classnames

(defn- classnames-from-files
  "Given a file relation, "
  [cp-relation file-relation]
  (let [classfiles (select file-relation (rlike ~name ".*\\.class$"))
        cp (field-seq cp-relation :path)]
    (pipe (project classfiles :path :name)
          (mapcat (fn [tup] (map #(str % "." (without-dotclass (:name tup)))
                                 (path->package cp (:path tup)))))
          (filter sym->class))))

;; assume classpath inside the jar to be .; (ignore the MANIFESTs classpath-line)
(defn- classnames-from-jars ;; assume jar-R contains only jars on the classpath
  [jar-R]
  (let [classfiles (field-seq (select jar-R (and (not ~directory) (rlike ~name ".*\\.class$")))
                              :name)]
    (map #(.replace #^String % \/ \.) classfiles)))

(defn- classnames-from-ns
  "Given the ns-imports relation, return all mentioned classes."
  [ns-imports-R]
  (let [classes (field-seq ns-imports-R :class)]
    (reduce conj  
            #{}
            classes
            ;(concat classes (mapcat supers classes))
            )))

(defn- find-classnames
  [ns-imports-R, file-R, jar-R, classpath-R]
  (clojure.set/union (classnames-from-ns ns-imports-R)
                     (classnames-from-files classpath-R file-R)
                     (classnames-from-jars jar-R)))

;; classes

(defn- class-tuple
  "Creates a tuple from a java.lang.Class object."
  [cname #^Class c]
  {:name cname
   :type (cond (.isEnum c) :enum
               (.isInterface c) :interface
               (try-ignore (.isAnonymousClass c)) :anonymous
               ;(.isLocalClass c) :local
               (.isPrimitive c) :primitive
               ;.isSynthetic c) :synthetic
               :else :class)
   :super (if-let [n (.getSuperclass c)]
            (class->sym n))
   :modifiers (.getModifiers c)})

(defn make-class-R [classname-seq]
  (make-relation (magic-map (fn ([] classname-seq)
                                ([cname] (class-tuple cname (sym->class cname)))))
                 :key :name
                 :fields (keys (class-tuple 'String String))))

(defn- class-interfaces [cname]
  (map (fn [#^Class i] {:interface (symbol (.getName i))
                        :class cname})
       (seq (.getInterfaces (sym->class cname)))))

(defn make-interface-R [classname-seq]
  (make-relation (magic-map (fn ([] (filter #(let [c (sym->class %)] (and c (not= 0 (count (.getInterfaces c)))))
                                            classname-seq))
                                ([cname] (set (class-interfaces cname)))))
                 :key :class
                 :fields [:interface :class]))

(defn- class-methods [cname]
  (map (fn [#^java.lang.reflect.Method m]
         {:class cname
          :declaring-class (symbol (.getName (.getDeclaringClass m)))
          :name (symbol (.getName m))
          :arity (count (try-ignore (.getParameterTypes m)))
          :returntype (try-ignore (class->sym (.getReturnType m)))
          :returns-array (try-ignore (.isArray (.getReturnType m)))
          :modifiers (.getModifiers m)})
       (seq (try-ignore (.getMethods #^Class (sym->class cname))))))

(defn make-method-R [classname-seq]
  (def _xxx classname-seq)
  (make-relation (magic-map (fn ([] (filter #(< 0 (count (try-ignore (.getMethods (sym->class %))))) classname-seq))
                                ([cname] (set (class-methods cname)))))
                 :key :cname
                 :fields (keys (first (class-methods 'java.lang.Object)))))

(defn- method-args [cname]
  (doall (mapcat (fn [#^java.lang.reflect.Method m]
                   (let [m-args (.getParameterTypes m)
                         m-args-count (count m-args)
                         m-name (symbol (.getName m))]
                     (map (fn [#^Class c p] {:class cname
                                             :method m-name
                                             :arity m-args-count
                                             :type (class->sym c)
                                             :position p})
                          m-args
                          (range m-args-count))))
                 (try-ignore (.getDeclaredMethods #^Class (sym->class cname))))))

(defn make-method-args-R [classnames]
  (make-relation (magic-map (fn ([] classnames)
                                ([cname] (set (method-args cname)))))
                 :key :class
                 :fields (keys (first (method-args 'java.lang.Object)))))


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

(defn make-file-relations [initial-paths file-filter-regex]
  (let [classpath-rel (make-classpath-R)
        file-rel (let [fr (make-file-R initial-paths)]
                   (if-let [rx file-filter-regex]
                     (select fr (rlike ~name rx))
                     fr))
        jar-rel (make-jar-R (select file-rel
                                    (and (rlike ~name ".*\\.jar$") ;; be shure to catch only jars on the classpath
                                         (contains? classpath-rel {:path (str ~path File/separator ~name)}))))]
    [classpath-rel file-rel jar-rel]))

(defn make-reflection-relations 
  ([& opts]
     (let [opts (as-keyargs opts {:files (concat (list-classpaths) (list-bootclasspaths))
                                  :filename-filter-regex nil
                                  :namespaces (all-ns)})

           ;; namespace relations (clojure)
           ns-rel (make-namespace-R (:namespaces opts))
           imports-rel (make-ns-imports-R ns-rel)
           aliases-rel (make-ns-aliases-R ns-rel)
           refers-rel (make-ns-refers-R ns-rel)
           interns-rel (make-ns-interns-R ns-rel)
           publics-rel (make-ns-publics-R ns-rel)
           
           ;; files
           [classpath-rel file-rel jar-rel] (memoize (make-file-relations (:files opts) (:filename-filter-regex opts)))
           ;; java reflection
           classnames (filter sym->class (find-classnames imports-rel file-rel jar-rel classpath-rel)) ;; only loadable classes
           class-rel (make-class-R classnames)
           method-rel (make-method-R classnames)
           method-args-rel (make-method-args-R classnames)
           interfaces-rel (make-interface-R classnames)]

       {:namespace ns-rel
        :imports imports-rel
        :aliases aliases-rel
        :refers refers-rel
        :interns interns-rel
        :publics publics-rel
        
        :classpaths classpath-rel
        :files file-rel
        :jars jar-rel

        :classes class-rel
        :interfaces interfaces-rel
        :method-rel method-rel
        :method-args method-args-rel})))

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




