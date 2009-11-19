(ns leiningen.minicheck
  (:use [minicheck :only [reset-all-properties run-all-properties]] 
        [clojure.contrib.java-utils :only [file]]
        [clojure.contrib.find-namespaces :only [find-namespaces-in-dir]]))

(defn minicheck [project & args]
  (reset-all-properties)
  (doseq [n (find-namespaces-in-dir (file (project :root) "check"))]
    (require n))
  (run-all-properties))