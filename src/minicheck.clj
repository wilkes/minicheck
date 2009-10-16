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

(ns minicheck)

(def *random* (java.util.Random.))
(def *all-properties* (atom []))

(defmulti arbitrary
  "Returns a generator function that will return arbitrary data."
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
(defmethod arbitrary :default  [_] #(. *random* nextDouble))

(defn elements
  "Creates a generator that will return a random item from the supplied collection."
  [coll]
  (let [g (arbitrary :int (count coll))]
    #(nth (vec coll) (g))))

(defn one-of
  "Creates a generator that will randomly use on of the supplied generators"
  [& gen-coll]
  (let [gens (elements gen-coll)]
    ;; nested the function to make the unwrapping a bit more clear
    (fn []
      (let [g (gens)]
        (g)))))

(defn such-that
  "Creates a generator that will return a value satisfying the supplied test function"
  [test-fn gen]
  #(first (filter test-fn (repeatedly gen))))

(defn vector-of
  "Creates a generator that will return a list of n values from the supplied generator"
  [n gen]
  #(take n (repeatedly gen)))

(defn list-of
  "Creates a generator that will return a list of at most n values from the supplied generator"
  [max-size gen]
  (let [length-gen (arbitrary :int max-size)]
    #((vector-of (length-gen) gen))))

(defn choose
  "Creates a generator that will return an int within the given range (inclusive)"
  [low high]
  (such-that (fn [i] (and (<= low i) (>= high i)))
             (arbitrary :int (inc high))))

(defn sample*
  "Returns a lazy sequence of n runs of the supplied generator (defaults 10)"
  ([gen]
     (sample* gen 10))
  ([gen n]
     (take n (repeatedly gen))))

(defn sample
    "Prints to *out* n runs of the supplied generator (defaults 10)"
  ([gen]
     (sample gen 10))
  ([gen size]
     (doseq [s (sample* gen size)]
       (prn s))))

(defn write-now [v]
  (doto *out* (.write (str v)) (.flush)))

(defn write-value [vs]
  (if (seq? vs)
    (do (write-now "[")
        (doseq [v (interpose " " vs)] (write-value v))
        (write-now "]"))
    (write-now vs)))

(defn write-failure [vs form]
  (write-now "\n")
  (write-now (str form))
  (write-now (str " failed with args "))
  (write-value vs)
  (write-now "\n"))

(defn property
  "Low-level property maker. Use the defprop macro instead."
  [name gens check form]
  (fn [& [n]]
    (write-now (str name ":\n"))
    (dotimes [c (if n n 100)]
      (let [vs (for [g gens] (g))]
        (try
         (write-now ".")
         (assert (apply check vs))
         (catch Exception e
           (write-failure vs form)
           (throw e)))))
       (write-now "\n")))

(defmacro defprop
  "Creates a new property and adds it to the *all-properties* collection"
  [name gen-bindings & body]
  (let [vars (map first (partition 2 gen-bindings))
        gens (map second (partition 2 gen-bindings))]
    `(do
       (def ~name
            (property ~(str name) [~@gens]
                      (fn [~@vars] ~@body)
                      (quote ~@body)))
       (swap! *all-properties* conj ~name))))


(defn run-all-properties
  "Runs each the property in the *all-properties* n times (default 100)"
  [& [n]]
  (doseq [f @*all-properties*] (f (if n n 100))))

(defn reset-all-properties
  "Clears out the *all-properties* collection."
  []
  (reset! *all-properties* []))

(defn is-in?
  "Does a linear search to test for membership in a seq"
  [v vs]
  (some #{true} (map #(= % v) vs)))