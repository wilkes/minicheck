(ns minicheck.checks
  (:use minicheck))

(reset-all-properties)

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

(defn in-range? [val lo hi]
  (and (<= lo val) (>= hi val)))

(defprop alpha-lower-property
    [c (arbitrary :alpha-lower)]
  (in-range? (int c) (int \a) (int \z)))

(defprop alpha-upper-property
    [c (arbitrary :alpha-upper)]
  (in-range? (int c) (int \A) (int \Z)))

(defprop number-char-property
    [c (arbitrary :number-char)]
  (in-range? (int c) (int \0) (int \9)))

(defprop alphanumeric-string-property
    [s (arbitrary :alphanumeric-string)]
  (every? #(or (in-range? (int %) (int \0) (int \9))
               (in-range? (int %) (int \a) (int \z))
               (in-range? (int %) (int \A) (int \Z)))
          s))

(run-all-properties)