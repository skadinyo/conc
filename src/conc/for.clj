(ns conc.for)

(defn one?
  [n]
  (or (= 1 n)
    (= "1" n)))

(defn reduce-
  ([f [x & xs]] (reduce- f x xs))
  ([f init args]
    (if (or (nil? args) (empty? args))
      init
      (reduce- f (f init (first args)) (rest args)))))

(fn reductions-
  ([f [x & xs]] (reductions- f x xs))
  ([f init [x & xs]]
    (if x
      (cons init (lazy-seq (reductions- f (f init x) xs)))
      [init])))

