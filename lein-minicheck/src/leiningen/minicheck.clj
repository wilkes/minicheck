(ns leiningen.minicheck
  (:require lancet)
  (:use [clojure.contrib.java-utils :only [file]]
        [clojure.contrib.find-namespaces :only [find-namespaces-in-dir]])
  (:import org.apache.tools.ant.taskdefs.Java
           (org.apache.tools.ant.types Environment$Variable Path)))

;; Stolen from leiningen.compile
(defn find-lib-jars
  "Returns a seq of Files for all the jars in the project's library directory."
  [project]
  (filter #(.endsWith (.getName %) ".jar")
          (file-seq (file (:library-path project)))))

;; Stolen from leiningen.compile
(defn make-path
  "Constructs an ant Path object from Files and strings."
  [& paths]
  (let [ant-path (Path. nil)]
    (doseq [path paths]
      (.addExisting ant-path (Path. nil (str path))))
    ant-path))

;; Stolen from leiningen.compile
(defn eval-in-project
  "Executes form in an isolated classloader with the classpath and compile path
  set correctly for the project."
  [project form]
  (let [java (Java.)]
    (.setProject java lancet/ant-project)
    (.setClasspath java (apply make-path
                               (:source-path project)
                               (:checks-path project) ;; only diff
                               (find-lib-jars project)))
    (.setClassname java "clojure.main")
    (.setValue (.createArg java) "-e")
    (.setValue (.createArg java) (prn-str form))
    (.execute java)))

(defn minicheck
  "Run minicheck for all the checks in the project.  Looks under checks/"
  [project]
  (let [project (merge project {:library-path (str (:root project) "/lib")
                                :source-path  (str (:root project) "/src")
                                :checks-path  (str (:root project) "/checks")})]
    (let [namespaces (find-namespaces-in-dir (file (:checks-path project)))]
      (eval-in-project
       project `(do
                  (require 'minicheck)
                  (minicheck/reset-all-properties)
                  (doseq [n# '~namespaces]
                    (require n#))
                  (minicheck/run-all-properties))))))