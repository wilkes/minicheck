(use 'minicheck)

(reset-all-properties)

;; Port of the quicksort examples from
;; http://book.realworldhaskell.org/read/testing-and-quality-assurance.html

(defmethod arbitrary ::list [_ size type]
  (list-of size (arbitrary type)))

(def int-list (arbitrary ::list 1000 :int))

(defprop sort-is-idempotent
    [xs int-list]
  (= (sort (sort xs)) (sort xs)))

(defprop sort-min-first
    [xs (such-that not-empty int-list)]
  (= (first (sort xs)) (apply min xs)))

(defn ordered? [[x & xs]]
  (cond
    (nil? x) true
    (nil? xs) true
    (< x (first xs)) (recur xs)))

(defprop sort-is-ordered
    [xs int-list]
  (ordered? (sort xs)))

(defn seq-diff [xs ys]
  (filter #(not (is-in? % xs)) ys))

(defn permuation? [xs ys]
  (and
   (empty? (seq-diff xs ys))
   (empty? (seq-diff ys xs))))

(defprop sort-is-permutation
    [xs int-list]
  (permuation? xs (sort xs)))

(defprop sort-max-last
    [xs (such-that not-empty int-list)]
  (= (last (sort xs)) (apply max xs)))

(defprop sort-concat
    [xs (such-that not-empty int-list)
     ys (such-that not-empty int-list)]
  (= (first (sort (concat xs ys)))
     (min (apply min xs) (apply min ys))))

(run-all-properties)