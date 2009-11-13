
;; example relations: reflection

;; TODO: make it fault tolerant
(ns hoeck.rel.reflection
  (:use hoeck.rel
        hoeck.library
        clojure.contrib.duck-streams
        clojure.contrib.pprint)
  (:import (java.io File, FileInputStream)
           (java.util.jar JarInputStream, JarEntry)
	   (java.util.regex Pattern)
           (java.lang.instrument IllegalClassFormatException)
	   (java.lang.reflect Modifier)))

(defmacro try-ignore
  "Execute body while ignoring all classloading errors"
  [& body]
  `(try ~@body
        (catch NullPointerException e# nil)
        (catch LinkageError e# nil)
        (catch IllegalClassFormatException e# nil)
        (catch ClassNotFoundException e# nil)
        (catch NoClassDefFoundError e# nil)
        (catch NullPointerException e# nil)))


;; helpers

(let [primitive-types {'boolean Boolean/TYPE
                       'int     Integer/TYPE
                       'float   Float/TYPE
                       'double  Double/TYPE
                       'byte    Byte/TYPE
                       'long    Long/TYPE
                       'char    Character/TYPE
                       'short   Short/TYPE
                       'void    Void/TYPE}] ;; or use nil for void??
  (def getclass
;;    "Converts a symbol or string to a java.lang.Class. Works for primitive types too.
;;  Returns nil if class named by s doesn't exist.."
       (memoize (fn [s]
                  (and s (or (primitive-types (symbol s))
                             (try-ignore (Class/forName (str s)))))))))

(def classname
;;  "Converts a Class to a Symbol. For arrays, returns the component name."
  (memoize (fn [#^Class c]
             (if (.isArray c)
               (classname (.getComponentType c))
               (.getName c)))))

(defn get-static-value [field]
  (let [parent (.getDeclaringClass field)]
    (when (and (java.lang.reflect.Modifier/isStatic (.getModifiers field))
	       (java.lang.reflect.Modifier/isPublic (.getModifiers field)))
      (.get field parent))))


;; fields

(defn class-fields
  "Return a relation of class fields"
  [classname]
  (when-let [c (getclass classname)]
    (set (map #(hash-map :name (.getName %)
                         :type (str (.getType %))
                         :modifiers (.getModifiers %)
                         :field %)
              (seq (.getFields c))))))

(defn class-static-fields
  "Return a relation of static fields and their values, names and types."
  [c]
  (project (select (class-fields (classname c)) (Modifier/isStatic ~modifiers))
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


;; namespaces

(defn namespaces [] (relation :name (map ns-name (all-ns))))

(defn fjoin-with-ns [ns-rel ns-f relation-f] (fjoin ns-rel #(let [a (-> % :name find-ns ns-f)] (relation-f a))))
(defn aliases [ns-rel] (fjoin-with-ns ns-rel ns-aliases #(relation :alias (keys %) :aliased-name (map ns-name (vals %)))))
(defn refers [ns-rel]  (fjoin-with-ns ns-rel ns-refers  #(relation :symbol (keys %) :var (vals %))))
(defn interns [ns-rel] (fjoin-with-ns ns-rel ns-interns #(relation :symbol (keys %) :var (vals %))))
(defn publics [ns-rel] (fjoin-with-ns ns-rel ns-publics #(relation :symbol (keys %) :var (vals %))))
(defn imports [ns-rel] (fjoin-with-ns ns-rel ns-imports #(relation :symbol (keys %) :class (map classname (vals %)))))


;; files

(defn classpaths []
  (union (project (relation :path (list-classpaths)) * [:normal :type])
         (project (relation :path (list-bootclasspaths)) * [:boot :type])))

(defn- file-tuple [#^File f]
  {:file (.getName f)
   :path (.getParent f)
   :time (.lastModified f)
   :directory (.isDirectory f)
   :size (.length f)})

(defn files
  "Given a relation containg a :path field, return a relation of all files and files below those paths
  using `clojure.core/file-seq'."
  [path-relation] 
  (fjoin path-relation 
	 #(let [f (-> % :path File.)]
	    (when (.exists f) (map file-tuple (file-seq f))))))

(defn- jar-entry-tuple [#^JarEntry je] ;; java.util.jar.jarEntry
  {:name (.getName je)
   :size (.getSize je)
   :compressed-size (.getCompressedSize je)
   :time (.getTime je)
   :comment (.getComment je)
   :directory (.isDirectory je)})

(defn- jar-entries
  "Return a seq of JarEntry(ies)."
  [pathname filename]
  (with-open [inp (JarInputStream. (FileInputStream. (str pathname File/separator filename)))]
    (doall (take-while identity (repeatedly #(.getNextJarEntry inp))))))

(defn jar? [s] (re-matches #"^.*\.jar$" s))

(defn jars
  "Given a file-relation (containing :path and :file fields), return a relation
  of all entries from jars mentioned in the file-relation"
  [file-relation]
  (fjoin (-> file-relation
	     (select (jar? ~file))
	     (project :path :file)
	     (rename :file :jar))
	 #(map jar-entry-tuple (jar-entries (:path %) (:jar %)))))


;; classes

(defn- classnames-from-jars [jars]
  (project (fjoin (select jars (re-matches #"^.*\.class$" ~name))
		  #(let [n (:name %)
			 p (.lastIndexOf (:name %) "/")]
		     [{:class (.replace (.substring n 0 (- (count n) 6)) \/ \.)}]))
	   :class :path :jar))

(let [;; sep-regex Pattern/quote File/separator) <- the good way
      sep-regex #"\\|/" ;; the bad way
      p (re-pattern sep-regex)]
  (defn tokenize-path [s]    
    (seq (.split p s))))

(defn match-heads [src comp] ;; (match-heads '(a b c) '(a)) -> (b c)
  (let [m (map = src comp)]
    (when (every? identity m)
      (drop (count m) src))))

(defn- classnames-from-classpaths [classpaths, files]  
  (project (fjoin (project (select files (re-matches #"^.*\.class$" ~file))
			   [(.substring ~file 0 (- (count ~file) 6)) :class]
			   [(tokenize-path ~path) :path])
		  #(relation :package (remove nil? (map (partial match-heads (:path %)) (map tokenize-path (map :path classpaths))))))
	   [(apply str (interpose "." (concat ~package [~class]))) :class]))

(defn classnames
  "Return a relation with names of all known classes."
  ([] (classnames (classpaths)))
  ([classpaths] (classnames classpaths (files classpaths)))
  ([classpaths files] (classnames classpaths files (jars (select files (jar? ~file)))))
  ([classpaths files jars]
     ;; try loading all classes from all known locations     
     (union (project (classnames-from-classpaths classpaths files) :class)
	    (project (classnames-from-jars jars) :class))))


;; reflection

(defn- class-tuple
  [classname]
  (let [c (getclass classname)]
    (when c {:class classname
             :type (cond (.isEnum c) :enum
                         (.isInterface c) :interface
                         (.isPrimitive c) :primitive
                         :else :plain)     
             :modifiers (.getModifiers c)})))

(defn classes [classnames]
  (->> classnames
       (map :class)
       (map class-tuple)
       (remove nil?)
       (set)))

(defn class-hierarchy [classes]
  (-> classes
      (project :class)
      (fjoin #(relation :super (map classname 
				    (remove nil? (let [c (getclass (:class %))]
                                                   (when c (cons (.getSuperclass c) (.getInterfaces c))))))))))

(defn- method-tuple [#^java.lang.reflect.Method m]
  {:method (symbol (.getName m))
   :type (classname (.getReturnType m))
   :array (.isArray (.getReturnType m))
   :args (map classname (.getParameterTypes m))
   :modifiers (.getModifiers m)})

(def __cm (atom {}))
(defn class-methods [classname]
  (reset! __cm classname)
  (fjoin (relation :class [classname])
	 #(map method-tuple (try-ignore (seq (.getDeclaredMethods (getclass (:class %))))))))

(defn all-methods [classnames]
  (fjoin classnames #(class-methods (:class %))))

(defn all-fields [classnames]
  (fjoin classnames #(class-fields (:class %))))

(defn make-reflection-relations []
  ;;"Loading my classpath (32K classes) needs about 150Mb of PermGen space and about 250MB Heap."
  (let [cp (classpaths)
	fs (files cp)
	js (jars (files (select cp (jar? ~path))))
	cn (select (classnames cp fs js) (not (re-matches #".*$fn__[0-9]+.*" ~class)))
	c (classes cn)
	ch (class-hierarchy c)
	m (all-methods cn)
	f (all-fields cn)]
    {:classpaths cp
     :files fs
     :jars js
     :classnames cn
     :classes c
     :class-hierarchy ch
     :methods m
     :fields f}))

(def reflection-relations (delay (make-reflection-relations)))

(defmacro with-relations [& body]
  `(binding [*relations* (merge *relations* (force reflection-relations))]
     ~@body))

(comment
  ;; some examples of using relations to query the java reflection interface
  
  
)

