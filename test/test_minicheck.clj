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

(let [run-count (atom nil)]
  (defcheck lifecycle-check
    {:before-all #(swap! run-count (fn [_] 0))
     :before #(swap! run-count inc)
     :after #(is (<= @run-count *test-run-count*))
     :after-all #(is (= @run-count *test-run-count*))}
    []
    (is (> @run-count 0))))

(let [cs (atom 0)]
  (defcheck eager-seq-of-is-eager
    {:after-all #(is (= @cs (* 10 *test-run-count*)))}
    [xs (arbitrary :eager-seq-of #(swap! cs inc) :exactly 10)]
    (is (> @cs 0))))


(let [cs (atom 0)]
  (defcheck seq-of-is-lazy
    {:after-all #(is (= @cs 0))}
    [xs (arbitrary :seq-of #(swap! cs inc) :exactly 10)]
    (is (= @cs 0))))