(ns conc.core)


(defn prime?
  [x]
  (cond
    (or (= 2 x)
      (= 3 x))
    true
    (or (> 2 x)
      (= 0 (rem x 2))
      (= 0 (rem x 3)))
    false
    :else
    (let [lim (int (Math/sqrt x))]
      (loop [i 5]
        (if (> i lim)
          true
          (if (= 0 (rem x i))
            false
            (recur (+' i 2))))))))

(defn next-prime
  [n]
  (let [i (if (even? n)
            (+ 1 n)
            (+ 2 n))]
    (loop [j i]
      (if (prime? j)
        j
        (recur (+ 2 j))))))


(defn primes-to
  [lim]
  (let [res (boolean-array (inc lim) true)
        par (int (Math/sqrt lim))]
    (do
      (doseq [i (range 2 lim)
              :while (<= i par)
              :when (aget res i)]
        (doseq [j (range (+ i i) lim i)]
          (aset res j false)))
      (filter #(aget res %)
        (range 2 lim)))))

(defn div-until
  [n pembagi]
  (loop [i n]
    (if (= 0 (rem i pembagi))
      (recur (quot i pembagi))
      i)))

(defn n-permutes
  [n coll]
  (let [init      (partition 1 coll)
        join-coll (fn [coll join]
                    (let [init-set (set (partition 1 coll))
                          join-set (set join)
                          diff     (vec (clojure.set/difference join-set init-set))]
                      (map (fn [i]
                             (concat coll
                               i))
                        diff)))]
    (cond
      (= 1 n) init
      (> n (count coll)) []
      :else (loop [c 1 res init]
              (if (= c n)
                res
                (recur (inc c)
                  (->> res
                    (map #(join-coll % init))
                    (flatten)
                    (partition (inc c)))))))))

(defn combinations
  "Generate the combinations of n elements from a list of [0..m)"
  [m n]
  (let [xs (range m)]
    (loop [i (int 0) res #{#{}}]
      (if (== i n)
        res
        (recur (+ 1 i)
               (set (for [x xs r res
                          :when (not-any? #{x} r)]
                      (conj r x))))))))

(defn coll-integer
  [coll]
  (let [c     (count coll)
        rcoll (vec (reverse coll))]
    (reduce +
      (map #(*' (reduce *' (repeat % 10))
             (get rcoll %))
        (range 0 c)))))

(defn number-collumn
  [n]
  (->> n
    (str)
    (seq)
    (map str)
    (map #(Integer/parseInt %))))

(defn number-coll
  [n]
  (let [refs 10]
    (loop [temp n res []]
      (let [r (rem temp refs)
            q (quot temp refs)]
        (if (= 0 q)
          (cons temp res)
          (recur (/ (- temp r) 10)
            (cons r res)))))))


(defn permutes?
  [coll]
  (apply = (map sort
             (map number-collumn
               coll))))

(defn pow
  "a^b"
  [a b]
  (reduce *' (repeat b a)))

(defn count-div
  [a b]
  (loop [temp b res 0]
    (if (not (= 0 (rem temp a)))
      res
      (recur (quot temp a) (inc res)))))