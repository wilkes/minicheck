(ns test-minicheck
  (:use clojure.test
        minicheck))

(defcheck elements-property
    [x (arbitrary :elements [1 2 3])]
    (is (is-in? x [1 2 3])))

(defcheck one-of-property
    [x (arbitrary :one-of
                  (elements [1 2 3])
                  (elements [true false]))]
    (is (or (is-in? x [1 2 3])
            (is-in? x [true false]))))

(defcheck such-that-property
    [b (arbitrary :such-that true? (arbitrary :bool))]
  (is b))

(defcheck seq-of-exactly-property
    [xs (arbitrary :seq-of (arbitrary :bool) :exactly 5)]
    (is (= (count xs) 5)))

(defcheck seq-of-max-property
    [xs (arbitrary :seq-of (arbitrary :bool) :max 5)]
    (is (<= (count xs) 5)))

(defcheck seq-of-min-property
    [xs (arbitrary :seq-of (arbitrary :bool) :min 5)]
    (is (>= (count xs) 5)))

(defcheck choose-property
    [x (arbitrary :choose 1 3)]
    (is (and (>= x 1) (<= x 3))))

(defcheck multi-arg-property
    [x (constantly 1)
     y (constantly 2)]
    (is (and (= x 1) (= y 2))))

(defn in-range? [val lo hi]
  (and (<= lo val) (>= hi val)))

(defcheck alpha-lower-property
    [c (arbitrary :alpha-lower-char)]
    (is (in-range? (int c) (int \a) (int \z))))

(defcheck alpha-upper-property
    [c (arbitrary :alpha-upper-char)]
    (is (in-range? (int c) (int \A) (int \Z))))

(defcheck number-char-property
    [c (arbitrary :numeric-char)]
    (is (in-range? (int c) (int \0) (int \9))))

(defcheck alphanumeric-string-property
    [s (arbitrary :string (seq-of (arbitrary :alphanumeric-char)
                                  :min-size 1 :max-size 36))]
    (is (every? #(or (in-range? (int %) (int \0) (int \9))
                     (in-range? (int %) (int \a) (int \z))
                     (in-range? (int %) (int \A) (int \Z)))
                s)))
