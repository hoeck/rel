
;; example relations: reflection

(ns hoeck.rel.reflection
  (:use hoeck.rel
        hoeck.library
        clojure.contrib.duck-streams
        clojure.contrib.pprint)
  (:import (java.io File, FileInputStream)
           (java.util.jar JarInputStream, JarEntry)
           (java.lang.instrument IllegalClassFormatException)
	   (java.lang.reflect Modifier)))

(defmacro try-ignore
  "Execute body while ignoring all classloading errors"
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
;;  Returns nil if class named by s doesn't exist.."
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

(defn get-static-value [field]
  (let [parent (.getDeclaringClass field)]
    (when (and (java.lang.reflect.Modifier/isStatic (.getModifiers field))
	       (java.lang.reflect.Modifier/isPublic (.getModifiers field)))
      (.get field parent))))

(defn class-fields
  "Return a relation of class fields"
  [c]
  (set (map #(hash-map :name (.getName %)
		       :type (str (.getType %))
		       :modifiers (.getModifiers %)
		       :field %)
	    (seq (.getFields c)))))

(defn class-static-fields
  "Return a relation of static fields and their values, names and types."
  [c]
  (project (select (class-fields c) (Modifier/isStatic ~modifiers))
	   *
	   [(get-static-value ~field) :static-value]))


;; modifiers 

(def modifiers (project (class-static-fields Modifier)
			:name 
			:static-value
			[(-> ~name .toLowerCase keyword) :keyword]))

(defn modifier-keys
  "given an integer, return a list of modifier keywords.
  (modifier-keys 25) -> (:final :static :public)"
  [i]
  (map :keyword
       (select (project modifiers :keyword [(not (zero? (bit-and ~static-value i))) :set?])
               ~set?)))

(defn modifier-int
  "Opposite function to modifier-keys. Given a list of modifier-keywords, return an integer."
  [& modifier-keys]
  (let [ms (set modifier-keys)]
    (->> (select modifiers (ms ~keyword))
         (map :static-value)
         (reduce +))))


;; classpath

(defn classpaths []
  (union (project (relation :path (list-classpaths)) * [:normal :type])
         (project (relation :path (list-bootclasspaths)) * [:boot :type])))


;; namespaces

(defn namespaces [] (project (relation [:ns] (all-ns)) :ns [(ns-name ~ns) :name]))
(defn aliases [nsr] (project (fjoin nsr #(relation [[:alias :full]] (ns-aliases (:ns %)))) :ns :alias :full))
(defn refers [nsr] (fjoin nsr #(relation [[:symbol :full]] (ns-refers (:ns %)))))
(defn interns [])
(defn publics [])
(defn imports [])

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
                         (select file-R (rlike ".*\\.jar$" ~name)))
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
  (let [classfiles (select file-relation (rlike ".*\\.class$" ~name))
        cp (field-seq cp-relation :path)]
    (pipe (project classfiles :path :name)
          (mapcat (fn [tup] (map #(str % "." (without-dotclass (:name tup)))
                                 (path->package cp (:path tup)))))
          (filter sym->class))))

;; assume classpath inside the jar to be .; (ignore the MANIFESTs classpath-line)
(defn- classnames-from-jars ;; assume jar-R contains only jars on the classpath
  [jar-R]
  (let [classfiles (field-seq (select jar-R (and (not ~directory) (rlike ".*\\.class$" ~name)))
                              :name)]
    (map #(.replace #^String (without-dotclass %) \/ \.) classfiles)))

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
  {:class cname
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
  (def _classname-seq classname-seq)
  (make-relation (magic-map (fn ([] classname-seq)
                                ([cname] (class-tuple cname (sym->class cname)))))
                 :key :class
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
         {:class cname ;; declaring-class
          ;:declaring-class (symbol (.getName (.getDeclaringClass m)))
          :name (symbol (.getName m))
          :arity (count (try-ignore (.getParameterTypes m)))
          :returntype (try-ignore (class->sym (.getReturnType m)))
          :returns-array (try-ignore (.isArray (.getReturnType m)))
          :modifiers (.getModifiers m)})
       (seq (try-ignore (.getDeclaredMethods #^Class (sym->class cname))))))

(defn make-method-R [classname-seq]
  (make-relation (magic-map (fn ([] (filter #(< 0 (count (try-ignore (.getDeclaredMethods (sym->class %))))) classname-seq))
                                ([cname] (set (class-methods cname)))))
                 :key :class
                 :fields (keys (first (class-methods 'java.lang.Object)))))

(defn- method-args [cname]
  (mapcat (fn [#^java.lang.reflect.Method m]
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
          (try-ignore (.getDeclaredMethods #^Class (sym->class cname)))))

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
    (field-seq (let [classes (project :classes :class)]
                 (difference (union (rename (project :interfaces :interface) :interface :class)
                                    (rename (project :imports :import) :import :class)
                                    (rename (project :method-args :type) :type :class)
                                    (rename (project :methods :returntype) :returntype :class))
                             classes))
                :class)))

(defn make-file-relations [initial-paths file-filter-regex]
  (let [classpath-rel (make-classpath-R)
        file-rel (let [fr (make-file-R initial-paths)]
                   (if-let [rx file-filter-regex]
                     (select fr (rlike rx ~name))
                     fr))
        jar-rel (make-jar-R (select file-rel
                                    (and (rlike ".*\\.jar$" ~name) ;; be shure to catch only jars on the classpath
                                         (contains? classpath-rel {:path (str ~path File/separator ~name)}))))]
    [classpath-rel file-rel jar-rel]))

(def make-file-relations-mem (memoize make-file-relations))

(defn make-reflection-relations
  "Loading all clojure&java6-classes (23K tuples) needs about 110Mb of PermGen space and about 35MB Heap."
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
           [classpath-rel file-rel jar-rel] (make-file-relations-mem (:files opts) (:filename-filter-regex opts))
           ;; java reflection
           classnames (map class->sym (remove nil? (map sym->class (find-classnames imports-rel file-rel jar-rel classpath-rel)))) ;; only loadable classes
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
        :methods method-rel
        :method-args method-args-rel})))

(defn load-reflection-relations
  ([path]
     (into {} (map #(vector (key %) (make-relation (with-in-reader (val %)) (read)))
                   (file-map path
                             #"^reflection-(.*)\.clj$")))))

(defn save-reflection-relations 
  ([path] (save-reflection-relations path *relations*))
  ([path relations] 
     (dorun (map #(save-relation (relations %)
                                 (str path File/separator "reflection-" (name %) ".clj"))
                 ;;(list :classpaths :files :jars :classes :interfaces :methods :method-args)
                 (keys *relations*)))
     nil))

(defn missing-classes? [relations]
  (< (count (:classes relations))
     (count (find-more-classes relations))))

(def reflection-relations (delay (make-reflection-relations)))

(defmacro with-relations [& body]
  `(binding [*relations* (merge *relations* (force reflection-relations))]
     ~@body))

(comment
;; some examples of using relations to query the java reflection interface

;; load many libs
(require '(clojure.contrib duck-streams sql command_line cond def duck_streams
                             except fcase import_static javalog lazy_seqs lazy_xml
                             mmap ns_utils seq_utils sql str_utils test_clojure
                             test_is trace zip_filter))

;; Loading all clojure&java6-classes (23K tuples) needs about 110Mb of PermGen space and about 35MB Heap.
;; increase PermGen space with the: "-XX:MaxPermSize=256m" jvm flag

;; print all accessible relations in a map rel-name -> fields
(with-relations (pprint (into {} (field-map (relations) :relation :field))))

;; example: private definitions in the hoeck.rel.reflection namespace
(with-relations (select (difference :interns :publics) (= ~ns 'hoeck.rel.reflection)))

;; class relation
(with-relations (*relations* :classes))

;; pretty-printing
(format "by default, only %d tuples of a relation are printed" (*pretty-print-relation-opts* :max-lines))

;; files on the classpath
(with-relations
 (order-by
  (project :files [(/ ~size 1000.0) :size-in-kb] :name :path)
  :size-in-kb
  '>))

;; jars
(with-relations (project (select :files (like '*.jar ~name)) :name :path))

;; jars that are mentioned in the classpath
(with-relations (join
                 (project (select :files (rlike ".*\\.jar$" ~name))
                          [(str ~path File/separator ~name) :path])
                 :path
                 :classpaths
                 :path))

;; relations are first-class
(do (with-relations (def java-lang-classes (select :classes (rlike "^java\\.lang\\.[A-Z].*" ~class))))
    (defn without-inner-classes [classes-rel]
      (select classes-rel (rlike "[^\\$]*" ~class))))

;; number of classes in the java.lang package
(count (without-inner-classes java-lang-classes))

;; all inner classes in the java.lang package
;; (takes some time to compute, calculates all indexes over the :classes relation due to poor difference implementation)
(count (difference java-lang-classes (without-inner-classes java-lang-classes)))

;; interfaces in the java lang package
(with-relations (select :classes (and (rlike "^java\\.lang\\.[A-Z].*" ~class) (= :interface ~type))))
;; or 
(select java-lang-classes (= :interface ~type))

;; definition of the interface relation
(with-relations (fields :interfaces))

;; number of implemented interfaces 
(with-relations (count (project :interfaces :interface)))

;; total number of interfaces
(with-relations (count (select :classes (= ~type :interface))))

;; all classes (directly) implementing clojure.lang.IFn
(with-relations (pprint (field-seq (select :interfaces (= 'clojure.lang.IFn ~interface)) :class)))

)




