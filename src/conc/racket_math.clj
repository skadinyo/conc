(ns conc.racket-math)

(defn divide?
  "Returns true if m divides n, false otherwise."
  [n m]
  (if (= 0 n)
    false
    (= 0 (rem m n))))

