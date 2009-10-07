(ns minicheck)

(def *random* (java.util.Random.))
(def *all-properties* (atom []))

(defmulti  generate (fn [& args] (first args)))

(defmethod generate :bool     [_] (. *random* nextBoolean))
(defmethod generate :double   [_] (. *random* nextDouble))
(defmethod generate :float    [_] (. *random* nextFloat))
(defmethod generate :gaussian [_] (. *random* nextGaussian))

(defmethod generate :int
  [_ & [size]]
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
     (fn [& [n]]
       (dotimes [c (if n n count)]
          (let [vs (for [g gens] (g))]
            (try
             (assert (apply check vs))
             (write-now ".")
             (catch Exception _
               (write-now (str "\n" vs "\n")))))))))

(defmacro defprop [name gen-bindings & body]
  (let [vars (map first (partition 2 gen-bindings))
        gens (map second (partition 2 gen-bindings))]
    `(do
       (def ~name
            (property [~@gens]
                      (fn [~@vars]
                        ~@body)))
       (swap! *all-properties* conj ~name))))

(defn is-in? [v vs]
  (some #{true} (map #(= % v) vs)))

(defprop elements-property
    [x (elements [1 2 3])]
  (is-in? x [1 2 3]))

(defprop one-of-property
    [x (one-of (elements [1 2 3])
             (elements [true false]))]
  (or (is-in? x [1 2 3])
      (is-in? x [true false])))

(defprop such-that-property
    [b (such-that true? (arbitrary :bool))]
  (true? b))

(defprop vector-of-property
    [xs (vector-of 5 (arbitrary :bool))]
  (= (count xs) 5))

(defprop list-of-property
    [xs (list-of 5 (arbitrary :bool))]
  (<= (count xs) 5))

(defprop choose-property
    [x (choose 1 3)]
  (and (>= x 1) (<= x 3)))

(defprop multi-arg-property
    [x (constantly 1)
     y (constantly 2)]
  (and (= x 1) (= y 2)))

(defn run-all-properties [& [n]]
  (doseq [f @*all-properties*] (f (if n n 100))))

