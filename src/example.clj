(use 'minicheck)

(reset-all-properties)

(defmethod arbitrary ::list [_ size type]
  (list-of size (arbitrary type)))

(defprop sort-is-idempotent
    [xs (arbitrary ::list 10 :int)]
  (= (sort (sort xs)) (sort xs)))

(defprop sort-min-first
    [xs (such-that not-empty (arbitrary ::list 10 :int))]
  (= (first (sort xs)) (apply min xs)))
