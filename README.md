Minicheck
====

Minicheck is an implementation of QuickCheck in clojure that runs with clojure.test.

Refer to the [example](http://github.com/wilkes/minicheck/blob/master/src/example.clj) which is a port of the quicksort example from [Chapter 11 of Real World Haskell](http://book.realworldhaskell.org/read/testing-and-quality-assurance.html)

Install
-------
1. Copy minicheck.clj into your project

Installing with Leiningen
----------
1. Install [leiningen](http://github.com/technomancy/leiningen)
1. Add minicheck to your project.clj 
        (defproject your-project "0.0.0-SNAPSHOT"
          :dependencies [[org.clojure/clojure "1.1.0-alpha-SNAPSHOT"]
                         [org.clojure/clojure-contrib "1.0-SNAPSHOT"]]
          :dev-dependencies [[minicheck "0.3.0"]])
1. Run with:
        lein test
