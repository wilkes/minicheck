Minicheck Leiningen Plugin
====

Installing
----------
1. Install [leiningen](http://github.com/technomancy/leiningen)
1. Install minicheck
1. Install lein-minicheck
1. Add the following to your lein script
    if [ "$1" = "minicheck" ]; then
        CLASSPATH=check/:$CLASSPATH
    fi
1. Modify your project's project.clj to minicheck and lein-minicheck
    (defproject your-project "0.0.0-SNAPSHOT"
      :dependencies [[org.clojure/clojure "1.1.0-alpha-SNAPSHOT"]
                     [org.clojure/clojure-contrib "1.0-SNAPSHOT"]]
      :dev-dependencies [[minicheck "0.0.0-SNAPSHOT"]
                         [lein-minicheck "0.0.0-SNAPSHOT"]])
1. Place your checks in a check/ folder in your project
1. Run: lein minicheck