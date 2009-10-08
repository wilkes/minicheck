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

(run-all-properties)