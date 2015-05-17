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