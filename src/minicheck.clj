;; The MIT License
;;  
;; Copyright (c) 2009 Wilkes Joiner
;;  
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;  
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;  
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
;; THE SOFTWARE.

(ns minicheck
  (:use clojure.test
        clojure.contrib.pprint)
  (:import java.util.Calendar
           java.text.SimpleDateFormat))

(def *random* (java.util.Random.))

(defmulti arb
  "Returns a function that will return arbitrary data."
  (fn [& args] (first args)))


(defmacro defarbfn [name-kw args & body]
  `(defmethod arb ~name-kw [kw# ~@args]
     (fn []
       ~@body)))

(defmacro defarb [name-kw args & body]
  `(defmethod arb ~name-kw [kw# ~@args]
     ~@body))

(defmacro gen [& params]
  `((arb ~@params)))

(defarbfn :bool     []     (. *random* nextBoolean))
(defarbfn :double   []     (. *random* nextDouble))
(defarbfn :float    []     (. *random* nextFloat))
(defarbfn :gaussian []     (. *random* nextGaussian))
(defarbfn :int [& [size]]  (if size
                             (. *random* nextInt size)
                             (. *random* nextInt)))
(defarbfn :long     []     (. *random* nextLong))

(defn elements
  "Arbitrary combinator that will return a random item from the supplied collection."
  [coll]
  (let [g (arb :int (count coll))]
    #(nth (vec coll) (g))))

(defn one-of
  "Arbitrary combinator that will randomly use on of the supplied arbitrar"
  [& arb-coll]
  (let [arbs (elements arb-coll)]
    ;; nested the function to make the unwrapping a bit more clear
    (fn []
      (let [g (arbs)]
        (g)))))

(defn such-that
  "Arbitrary combinator that will return a value satisfying the supplied test function"
  [test-fn arb]
  #(first (filter test-fn (repeatedly arb))))

(defn choose
  "Arbitrary combinator that will return an int within the given range (inclusive)"
  [low high]
  (such-that (fn [i] (and (<= low i) (>= high i)))
             (arb :int (inc high))))

(defn seq-of
  "Arbitrary combinator that will return a seq of values from the supplied arbitrary.  Options:
:exactly - to specify that the seq always be of the same size
:min - defaults to 0, ignored if :exactly is supplied
:max - defaults to 100, ignored if :exactly is supplied "
  [arb & options]
  (let [defaults {:min 0 :max 100}
        options (merge defaults (apply hash-map options))
        arb-size (if (options :exactly)
                   (constantly (options :exactly))
                   (choose (options :min) (options :max)))]
    #(take (arb-size) (repeatedly arb))))

(defn eager-seq-of [arb & options]
  #(doall ((apply seq-of arb options))))

;; These exist for consistency and so that you can call the gen macro
;; for the arbitrary combinators

(defarb :elements [& [coll]]
  (elements coll))

(defarb :one-of [& gen-coll]
  (apply one-of gen-coll))

(defarb :such-that [& [test-fn arb]]
  (such-that test-fn arb))

(defarb :choose [& [low high]]
  (choose low high))

(defarb :seq-of [& options]
  (apply seq-of options))

(defarb :eager-seq-of [& options]
  (apply eager-seq-of options))


;; Character and string arbitraries

(defarbfn :character [& [low high]]
  (char (gen :choose (or low 32) (or high 127))))

(defarb :alpha-lower-char []
  (arb :character (int \a) (int \z)))

(defarb :alpha-upper-char []
  (arb :character (int \A) (int \Z)))

(defarb :numeric-char []
  (arb :character (int \0) (int \9)))

(defarb :alphanumeric-char []
  (one-of (arb :alpha-lower-char)
          (arb :alpha-upper-char)
          (arb :numeric-char)))

(defarbfn :string [arb-characters]
  (apply str (arb-characters)))

;; Arbitrary Dates
(defn date
  "A convenience constructor for making a java.util.Date.  Takes zero or more 
   args. Zero args return the current time. One or more args returns a date 
   values provided and all other values zeroed out.
   Args are year, month, date, hour, minute, second, and  milliseconds"
  [& [year month date hour minute second milli]]
  (let [c  (Calendar/getInstance)
        z? #(int (or % 0))]
    (when year
      (.set c Calendar/YEAR        (z? year))
      (.set c Calendar/MONTH       (z? (- (or month 1) 1)))
      (.set c Calendar/DATE        (z? date))
      (.set c Calendar/HOUR_OF_DAY (z? hour))
      (.set c Calendar/MINUTE      (z? minute))
      (.set c Calendar/SECOND      (z? second))
      (.set c Calendar/MILLISECOND (z? milli)))
    (.getTime c)))

(defn add-days [number-of-days a-date]
  (.getTime (doto (Calendar/getInstance)
              (.setTime a-date)
              (.add Calendar/DATE number-of-days))))

(defn date-range [start end]
  (let [days (iterate (partial add-days 1) start)]
    (for [d days :while (<= (.getTime d) (.getTime end))]
      d)))

(defarb :date-in [start end]
  (elements (date-range start end)))

(defn sample*
  "Returns a lazy sequence of n runs of the supplied arb (defaults 10)"
  ([arb]
     (sample* arb 10))
  ([arb n]
     (take n (repeatedly arb))))

(defn sample
    "Prints to *out* n runs of the supplied arb (defaults 10)"
  ([arb]
     (sample arb 10))
  ([arb size]
     (doseq [s (sample* arb size)]
       (prn s))))

(defn is-in?
  "Does a linear search to test for membership in a seq"
  [v vs]
  (some #{true} (map #(= % v) vs)))

(def *test-run-count* 100)

(defn with-run-count [n prop]
  (binding [*test-run-count* n] (prop)))

(defmacro defcheck [name & args]
  (let [[arb-bindings body] (if (map? (first args))
                              [(second args) (nthnext args 2)]
                              [(first args) (nthnext args 1)])
        arb-bindings (vec (mapcat (fn [[v arb]]
                                    `[~v (~arb)])
                                  (partition 2 arb-bindings)))
        maybe-run (fn [k] `(if ~(k (first args))
                             (apply ~(k (first args)) [])))
        check-fn `(fn []
                    ~(maybe-run :before)
                    (let [~@arb-bindings]
                      (try ~@body
                       (finally ~(maybe-run :after)))))
        test-fn `(fn []
                   ~(maybe-run :before-all)
                   (try
                    (doseq [pass?#
                            (take *test-run-count*
                                  (repeatedly ~check-fn))
                            :while pass?#]
                      (print "."))
                    (finally
                     ~(maybe-run :after-all))))]
    (when *load-tests*
      `(def ~(vary-meta name assoc :test test-fn)
            #(test-var (var ~name))))))