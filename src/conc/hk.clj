(ns conc.hk)

;;use only inc dec defn partmatch cons

(defn add
  [a b]
  (cond
    (= 0 b) a
    (= 0 a) b
    (> b 0) (add (inc a) (dec b))
    :else (add (dec a) (inc b))))

(defn mul
  [a b]
  )

(defn take-
  [n [x & xs]]
  (if (= n 0)
    []
    (cons x (take- xs))))

(defn drop-
  [n [x & xs]]
  (if (= 0 n)
    xs
    (drop- xs)))

(defn concat-
  [[x & xs] ys]
  (if x
    (cons x (concat xs ys))))

(defn reverse-
  [x & xs]
  )

(defn last-
  [[x & xs]]
  (if xs
    (last- xs)
    x))

(defn init-
  [[x & xs]]
  (if xs
    (cons x (init- xs))
    []))

(defn take-while-
  [f [x & xs]]
  (if (f x)
    (cons x (take-while- f xs))
    []))

(defn drop-while-
  [f [x & xs]]
  (if (f x)
    (drop-while- f xs)
    xs))

(defn map-
  [f [x & xs]]
  (if x
    (cons (f x) (map- f xs))
    []))

(defn filter-
  [f [x & xs]]
  (if x
    (if (f x)
      (cons x (filter- f xs))
      (filter- f xs))
    []))

(defn remove-
  [f [x & xs]]
  (if x
    (if (f x)
      (remove- f xs)
      (cons x (remove- f xs)))
    []))

(defn max-
  [[x & xs]]
  (if xs
    (let [y (max- xs)]
      (if (> x y)
        x
        y))
    x))

(defn min-
  [[x & xs]]
  (if xs
    (let [y (min- xs)]
      (if (> x y)
        y
        x))
    x))

(defn max-by-
  [f [x & xs]]
  (if xs
    (let [y (max-by- f xs)]
      (if (> x y)
        x
        y))
    (f x)))

(defn min-by-
  [f [x & xs]]
  (if xs
    (let [y (min-by- f xs)]
      (if (> x y)
        y
        x))
    (f x)))

(defn delete-1-
  [n [x & xs]]
  (if x
    (if (= n x)
      xs
      (cons x (delete-1- n xs)))
    []))

(defn delete-all-
  [n [x & xs]]
  (if x
    (if (= n x)
      (delete-all- n xs)
      (cons x (delete-all- n xs)))
    []))

(defn all-
  [f [x & xs]]
  (if x
    (if (f x)
      (all- f xs)
      false)
    true))

(defn any-
  [f [x & xs]]
  (if x
    (if (f x)
      true
      (any- f xs))
    false))

(defn sum-
  [])
(defn prod-
  [])
(defn folds- [])
(defn scanls- [])
(defn nth- [])
(defn repeat-
  [n x]
  (if (= 0 n)
    []
    (cons x (repeat (dec n) x))))

(defn cycle-
  [coll]
  (lazy-seq (concat coll
                    (cycle- coll))))

(defn mapcat- [])

(defn distinct- [])

(defn partition- [])
(defn partition-by- [])
(defn frequencies- [])
(defn add- [])
(defn subs- [])
(defn expt- [])
(defn mul- [])
(defn div- [])
(defn rem- [])
(defn faktorial- [])
(defn fib- [])
(defn pascal- [])
(defn prime- [])
(defn range- [])
(defn interleave- [])
(defn unzip- [])
(defn take-last- [])
(defn drop-last- [])
(defn split-at- [])
(defn qsort- [])
(defn isort- [])
(defn modex [])
(defn divisor- [])
(defn pfactor- [])
