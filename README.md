Minicheck
====

Minicheck is an implementation of QuickCheck in clojure.

Refer to the [example](http://github.com/wilkes/minicheck/blob/master/src/example.clj) which is a port of the quicksort example from [Chapter 11 of Real World Haskell](http://book.realworldhaskell.org/read/testing-and-quality-assurance.html)

Install
-------
1. Copy minicheck.clj into your project

Installing with Leiningen
----------
1. Install [leiningen](http://github.com/technomancy/leiningen)
1. Install minicheck
        lein compile && lein install
1. Install lein-minicheck
        cd lein-minicheck/
        lein deps && lein compile && lein install   
1. Add the following to your lein script
        if [ "$1" = "minicheck" ]; then
            CLASSPATH=check/:$CLASSPATH
        fi
   This needs to be placed before the JVM launches as noted in the lein script.
1. Modify your project's project.clj to minicheck and lein-minicheck
        (defproject your-project "0.0.0-SNAPSHOT"
          :dependencies [[org.clojure/clojure "1.1.0-alpha-SNAPSHOT"]
                         [org.clojure/clojure-contrib "1.0-SNAPSHOT"]]
          :dev-dependencies [[minicheck "0.0.0-SNAPSHOT"]
                             [lein-minicheck "0.0.0-SNAPSHOT"]])
1. Place your checks in a check/ folder in your project
1. Run with:
        lein minicheck

Contributing
-----------
1. Install with Leiningen
2. Run
        lein deps
3. Hack and run lein minicheck often!