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
  (:use clojure.test))

(def *random* (java.util.Random.))

(defmulti arbitrary
  "Returns a function that will return arbitrary data."
  (fn [& args] (first args)))

(defmethod arbitrary :bool     [_] #(. *random* nextBoolean))
(defmethod arbitrary :double   [_] #(. *random* nextDouble))
(defmethod arbitrary :float    [_] #(. *random* nextFloat))
(defmethod arbitrary :gaussian [_] #(. *random* nextGaussian))

(defmethod arbitrary :int
  [_ & [size]]
  (if size
    #(. *random* nextInt size)
    #(. *random* nextInt)))

(defmethod arbitrary :long     [_] #(. *random* nextLong))

(defn elements
  "Arbitrary combinator that will return a random item from the supplied collection."
  [coll]
  (let [g (arbitrary :int (count coll))]
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
             (arbitrary :int (inc high))))

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


;; These exist for consistency and so that you can call the gen macro
;; for the arbitrary combinators

(defmethod arbitrary :elements [_ & [coll]]
  (elements coll))

(defmethod arbitrary :one-of [_ & gen-coll]
  (apply one-of gen-coll))

(defmethod arbitrary :such-that [_ & [test-fn arb]]
  (such-that test-fn arb))

(defmethod arbitrary :choose [_ & [low high]]
  (choose low high))

(defmethod arbitrary :seq-of [_ & options]
  (apply seq-of options))


;; Character and string arbitraries

(defmethod arbitrary :character [_ & [low high]]
  (let [low  (if low low 32)
        high (if high high 127)  ;; default to basic Latin characters
        arb-int (choose low high)]
    #(char (arb-int))))

(defmethod arbitrary :alpha-lower-char [_]
  (arbitrary :character (int \a) (int \z)))

(defmethod arbitrary :alpha-upper-char [_]
  (arbitrary :character (int \A) (int \Z)))

(defmethod arbitrary :numeric-char [_]
  (arbitrary :character (int \0) (int \9)))

(defmethod arbitrary :alphanumeric-char [_]
  (one-of (arbitrary :alpha-lower-char)
          (arbitrary :alpha-upper-char)
          (arbitrary :numeric-char)))

(defmethod arbitrary :string [_ arb-characters]
  #(apply str (arb-characters)))


(defn sample*
  "Returns a lazy sequence of n runs of the supplied arbitrary (defaults 10)"
  ([arb]
     (sample* arb 10))
  ([arb n]
     (take n (repeatedly arb))))

(defn sample
    "Prints to *out* n runs of the supplied arbitrary (defaults 10)"
  ([arb]
     (sample arb 10))
  ([arb size]
     (doseq [s (sample* arb size)]
       (prn s))))

(defmacro gen [& params]
  `((arbitrary ~@params)))

(defn is-in?
  "Does a linear search to test for membership in a seq"
  [v vs]
  (some #{true} (map #(= % v) vs)))

(def *test-run-count* 100)

(defn with-run-count [n prop]
  (binding [*test-run-count* n] (prop)))

(defmacro defcheck [name & args]
  (let [pre (:pre (first args))
        [arb-bindings body] (if pre
                              [(second args) (nthnext args 2)]
                              [(first args) (nthnext args 1)])
        check-fn `(fn []
                    (do
                      (if ~pre ~pre)
                      (let [~@arb-bindings]
                        ~@body)))]
    (when *load-tests*
      `(def ~(vary-meta name assoc :test
                        `#(doseq [pass?#
                                  (take *test-run-count*
                                        (repeatedly ~check-fn))
                                  :while pass?#]
                            (print ".")))
            #(test-var (var ~name))))))