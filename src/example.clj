(use 'clojure.test)
(use 'minicheck)

;; Port of the quicksort examples from
;; http://book.realworldhaskell.org/read/testing-and-quality-assurance.html

(defarb ::list [] (seq-of (arb :int) :exactly 10))

(defcheck sort-is-idempotent
  [xs (arb ::list)]
  (is (= (sort (sort xs)) (sort xs))))

(defcheck sort-min-first
  [xs (arb :such-that not-empty (arb ::list))]
  (is (= (first (sort xs)) (apply min xs))))

(defn ordered? [[x & xs]]
  (cond
   (nil? x) true
   (nil? xs) true
   (< x (first xs)) (recur xs)))

(defcheck sort-is-ordered
  [xs (arb ::list)]
  (is (ordered? (sort xs))))

(defn seq-diff [xs ys]
  (remove #(is-in? % xs) ys))

(defn permuation? [xs ys]
  (and
   (empty? (seq-diff xs ys))
   (empty? (seq-diff ys xs))))

(defcheck sort-is-permutation
  [xs (arb ::list)]
  (is (permuation? xs (sort xs))))

(defcheck sort-max-last
  [xs (arb :such-that not-empty (arb ::list))]
  (is (= (last (sort xs)) (apply max xs))))

(defcheck sort-concat
  [xs (arb :such-that not-empty (arb ::list))
   ys (arb :such-that not-empty (arb ::list))]
  (is (= (first (sort (concat xs ys)))
         (min (apply min xs) (apply min ys)))))
