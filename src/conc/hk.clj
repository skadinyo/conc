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
  (cond
    (= b 0) 0
    (= a 0) 0
    (= b 1) a
    ))