(ns test-minicheck
  (:use clojure.test
        minicheck))

(defcheck elements-property
  [x (gen :elements [1 2 3])]
  (is (is-in? x [1 2 3])))

(defcheck one-of-property
  [x (gen :one-of
          (elements [1 2 3])
          (elements [true false]))]
  (is (or (is-in? x [1 2 3])
            (is-in? x [true false]))))

(defcheck such-that-property
  [b (gen :such-that true? (arbitrary :bool))]
  (is b))

(defcheck seq-of-exactly-property
  [xs (gen :seq-of (arbitrary :bool) :exactly 5)]
  (is (= (count xs) 5)))

(defcheck seq-of-max-property
  [xs (gen :seq-of (arbitrary :bool) :max 5)]
  (is (<= (count xs) 5)))

(defcheck seq-of-min-property
  [xs (gen :seq-of (arbitrary :bool) :min 5)]
  (is (>= (count xs) 5)))

(defcheck choose-property
  [x (gen :choose 1 3)]
  (is (and (>= x 1) (<= x 3))))

(defcheck multi-arg-property
  [x ((constantly 1))
   y ((constantly 2))]
  (is (and (= x 1) (= y 2))))

(defn in-range? [val lo hi]
  (and (<= lo val) (>= hi val)))

(defcheck alpha-lower-property
  [c (gen :alpha-lower-char)]
  (is (in-range? (int c) (int \a) (int \z))))

(defcheck alpha-upper-property
  [c (gen :alpha-upper-char)]
  (is (in-range? (int c) (int \A) (int \Z))))

(defcheck number-char-property
  [c (gen :numeric-char)]
  (is (in-range? (int c) (int \0) (int \9))))

(defcheck alphanumeric-string-property
  [s (gen :string (seq-of (arbitrary :alphanumeric-char)
                          :min-size 1 :max-size 36))]
  (is (every? #(or (in-range? (int %) (int \0) (int \9))
                   (in-range? (int %) (int \a) (int \z))
                   (in-range? (int %) (int \A) (int \Z)))
              s)))
