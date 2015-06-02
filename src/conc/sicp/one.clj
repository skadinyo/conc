(ns conc.sicp.one)

(defn abs
  [x]
  (if (> x 0)
    x
    (* -1 x)))

(defn perfect?
  [n target]
  (let [n-int (int n)]
    (if (= target (* n-int n-int))
      n-int
      n)))

(defn sqrt
  ([x] (sqrt 1 x))
  ([guess x]
    (let [error (- x (* guess guess))]
      (if (<= (abs error) 0.00001)
        (perfect? guess x)
        (sqrt (/ (+ guess (/ x guess)) 2.0) x)))))

