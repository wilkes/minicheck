(ns minicheck)

(def *random* (java.util.Random.))

(defmulti  arbitrary :for)

(defmethod arbitrary :bool     [_] (. *random* nextBoolean))
(defmethod arbitrary :double   [_] (. *random* nextDouble))
(defmethod arbitrary :float    [_] (. *random* nextFloat))
(defmethod arbitrary :gaussian [_] (. *random* nextGaussian))

(defmethod arbitrary :int
  [arg-map]
  (if (:size arg-map)
    (. *random* nextInt (:size arg-map))
    (. *random* nextInt)))

(defmethod arbitrary :long     [_] (. *random* nextLong))
(defmethod arbitrary :default  [_] (. *random* nextDouble))

(defn generator [arg-map] #(arbitrary arg-map))

(defn elements [coll]
  (let [g (generator {:for :int
                      :size (count coll)})]
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
  (let [length-gen (generator {:for :int
                               :size max-size})]
    #((vector-of (length-gen) gen))))

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
     (property (generator {:for :int}) check))
  ([gen check]
     (property 100 gen check))
  ([count gen check]
     #(dotimes [c count]
        (let [v (gen)]
          (try
           (assert (check v))
           (write-now ".")
           (catch Exception _
             (write-now (str "\n" v "\n"))))))))

(defn is-in? [v vs]
  (some #{true} (map #(= % v) vs)))

(def elements-property
     (let [vs #{1 2 3}]
       (property (elements vs)
                 #(is-in? % vs))))

(def one-of-property
     (property (one-of (elements #{1 2 3})
                       (elements #{true false}))
               #(or (is-in? % #{1 2 3})
                    (is-in? % #{true false}))))

(def such-that-property
     (property (such-that true? (generator {:for :bool}))
               true?))

(def vector-of-property
     (property (vector-of 5 (generator {:for :bool}))
               #(= 5 (count %))))

(def list-of-property
     (property (list-of 5 (generator {:for :bool}))
               #(>= 5 (count %))))

(defn run-properties []
  (doseq [f [elements-property
             one-of-property
             such-that-property
             vector-of-property
             list-of-property]]
    (f)))

(defn run-samples []
  (sample
   (list-of 5 (generator {:for :int})))
  (sample
   (such-that not-empty (list-of 5 (generator {:for :int}))))
  (sample
   (one-of (generator {:for :int}) (generator {:for :bool})))
  ((property (list-of 10 (generator {:for :int}))
              #(= % (reverse (reverse %))))))
