(ns minicheck)

(def *random* (java.util.Random.))

(defmulti  generate (fn [& args] (first args)))

(defmethod generate :bool     [_] (. *random* nextBoolean))
(defmethod generate :double   [_] (. *random* nextDouble))
(defmethod generate :float    [_] (. *random* nextFloat))
(defmethod generate :gaussian [_] (. *random* nextGaussian))

(defmethod generate :int
  [_ size]
  (if size
    (. *random* nextInt size)
    (. *random* nextInt)))

(defmethod generate :long     [_] (. *random* nextLong))
(defmethod generate :default  [_] (. *random* nextDouble))

(defn arbitrary [& args] #(apply generate args))

(defn elements [coll]
  (let [g (arbitrary :int (count coll))]
    #(nth (vec coll) (g))))

(defn one-of [& gen-coll]
  (let [gens (elements gen-coll)]
    ;; nested the function to make the unwrapping a bit more clear
    (fn []
      (let [g (gens)]
        (g)))))

(defn such-that [test-fn gen]
  #(first (filter test-fn (repeatedly gen))))

(defn vector-of [n gen]
  #(take n (repeatedly gen)))

(defn list-of [max-size gen]
  (let [length-gen (arbitrary :int max-size)]
    #((vector-of (length-gen) gen))))

(defn choose [low high]
  (such-that (fn [i] (and (<= low i) (>= high i)))
             (arbitrary :int (inc high))))

(defn sample*
  ([gen]
     (sample* gen 10))
  ([gen size]
     (take size (repeatedly gen))))

(defn sample
  ([gen]
     (sample gen 10))
  ([gen size]
     (doseq [s (sample* gen size)]
       (prn s))))

(defn write-now [v]
  (doto *out* (.write v) (.flush)))

(defn property
  ([check]
     (property [(arbitrary :int)] check))
  ([gens check]
     (property 100 gens check))
  ([count gens check]
     #(dotimes [c count]
        (let [vs (for [g gens] (g))]
          (try
           (assert (apply check vs))
           (write-now ".")
           (catch Exception _
             (write-now (str "\n" vs "\n"))))))))

(defn is-in? [v vs]
  (some #{true} (map #(= % v) vs)))

(def elements-property
     (property [(elements [1 2 3])]
               #(is-in? % [1 2 3])))

(def one-of-property
     (property [(one-of (elements [1 2 3])
                         (elements [true false]))]
               #(or (is-in? % [1 2 3])
                    (is-in? % [true false]))))

(def such-that-property
     (property [(such-that true? (arbitrary :bool))]
               true?))

(def vector-of-property
     (property [(vector-of 5 (arbitrary :bool))]
               #(= (count %) 5)))

(def list-of-property
     (property [(list-of 5 (arbitrary :bool))]
               #(<= (count %) 5)))

(def choose-property
     (property [(choose 1 3)]
               #(and (>= % 1) (<= % 3))))

(def multi-arg-property
     (property 10 [(constantly 1) (constantly 2)]
               (fn [x y] (and (= x 1) (= y 2)))))

(defn run-properties []
  (doseq [f [elements-property
             one-of-property
             such-that-property
             vector-of-property
             list-of-property
             choose-property
             multi-arg-property]]
    (f)))

(defn run-samples []
  (sample
   (list-of 5 (arbitrary :int)))
  (sample
   (such-that not-empty (list-of 5 (arbitrary :int))))
  (sample
   (one-of (arbitrary :int) (arbitrary :bool)))
  ((property (list-of 10 (arbitrary :int))
              #(= % (reverse (reverse %))))))
