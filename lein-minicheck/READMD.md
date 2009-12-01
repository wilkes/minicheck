Minicheck Leiningen Plugin
====

Install
-------
1. Install [leiningen](http://github.com/technomancy/leiningen)
1. Modify your project's project.clj to minicheck and lein-minicheck
    (defproject your-project "0.0.0-SNAPSHOT"
      :dependencies [[org.clojure/clojure "1.1.0-alpha-SNAPSHOT"]
                     [org.clojure/clojure-contrib "1.0-SNAPSHOT"]]
      :dev-dependencies [[minicheck "0.2.0"]
                         [lein-minicheck "0.2.0"]
1. Place your checks in a checks/ folder in your project
1. Run: lein minicheck

Hacking
----------
1. Install [leiningen](http://github.com/technomancy/leiningen)
1. Run ./bin/install.sh
1. Hack...
1. Run: lein minicheck