(use 'clojure.test)
(use 'minicheck)

;; Port of the quicksort examples from
;; http://book.realworldhaskell.org/read/testing-and-quality-assurance.html

(defmethod arbitrary ::list [_]
  (seq-of (arbitrary :int) :exactly 10))

(defcheck sort-is-idempotent
  [xs (gen ::list)]
  (is (= (sort (sort xs)) (sort xs))))

(defcheck sort-min-first
  [xs (gen :such-that not-empty (arbitrary ::list))]
  (is (= (first (sort xs)) (apply min xs))))

(defn ordered? [[x & xs]]
  (cond
   (nil? x) true
   (nil? xs) true
   (< x (first xs)) (recur xs)))

(defcheck sort-is-ordered
  [xs (gen ::list)]
  (is (ordered? (sort xs))))

(defn seq-diff [xs ys]
  (remove #(is-in? % xs) ys))

(defn permuation? [xs ys]
  (and
   (empty? (seq-diff xs ys))
   (empty? (seq-diff ys xs))))

(defcheck sort-is-permutation
  [xs (gen ::list)]
  (is (permuation? xs (sort xs))))

(defcheck sort-max-last
  [xs (gen :such-that not-empty (arbitrary ::list))]
  (is (= (last (sort xs)) (apply max xs))))

(defcheck sort-concat
  [xs (gen :such-that not-empty (arbitrary ::list))
   ys (gen :such-that not-empty (arbitrary ::list))]
  (is (= (first (sort (concat xs ys)))
         (min (apply min xs) (apply min ys)))))
