(ns conc.core
  (require [clojure.core.reducers :as r]))

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

(defn pfactors
  [nt]
  (let [lim (Math/sqrt nt)]
    (loop [p 2 n nt res []]
      (let [d (quot n p) r (rem n p)]
        (if (> p lim)
          (sort (distinct (conj res p d)))
          (if (zero? r)
            (if (or (= 1 d) (prime? d))
              (sort (distinct (conj res p d)))
              (recur 2 d (conj res p)))
            (recur (next-prime p) n res)))))))

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

(defn dumb-prime
  [lim]
  (loop [i 2 temp (range 3 (inc lim)) res '()]
    (if i
      (recur (first temp)
             (rest (filter #(not= 0 (rem % i)) temp))
             (cons i res))
      res)))

(defn join-combi
  [init coll]
  (let [n (clojure.set/difference coll init)]
    (set (map #(conj init %) n))))

(defn combi
  [n coll]
  (if (zero? n)
    #{}
    (let [init (set (map set (partition 1 coll)))]
      (loop [i 1 res init]
        (if (= n i)
          res
          (recur (inc i)
                 (->> res
                      (map #(join-combi % coll))
                      (map set)
                      (apply clojure.set/union))))))))

(defn totient-to
  [lim]
  (let [p (set (primes-to lim))
        tot (int-array (range 0 lim))]
    (do
      (doseq [ip p]
        (doseq [i (range (* 2 ip) lim ip)]
          (aset tot i (int (* (aget tot i)
                              (- 1 (/ 1 ip)))))))
      (map (fn [i]
             (if (p i)
               [i (dec i)]
               [i (aget tot i)]))
           (range 1 lim)))))

(defn div-until
  [n pembagi]
  (loop [i n]
    (if (= 0 (rem i pembagi))
      (recur (quot i pembagi))
      i)))

(defn n-permutes
  [n coll]
  (let [init (partition 1 coll)
        join-coll (fn [coll join]
                    (let [init-set (set (partition 1 coll))
                          join-set (set join)
                          diff (vec (clojure.set/difference join-set init-set))]
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

(defn coll-integer
  [coll]
  (let [c (count coll)
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
    (if (= 0 (rem temp a))
      (recur (quot temp a) (inc res))
      res)))

(defn psquare?
  [n]
  (let [sn (Math/sqrt n)]
    (== sn (int sn))))


