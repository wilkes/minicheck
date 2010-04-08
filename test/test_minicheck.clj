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
    [xs (gen :eager-seq-of #(swap! cs inc) :exactly 10)]
    (is (> @cs 0))))


(let [cs (atom 0)]
  (defcheck seq-of-is-lazy
    {:after-all #(is (= @cs 0))}
    [xs (gen :seq-of #(swap! cs inc) :exactly 10)]
    (is (= @cs 0))))