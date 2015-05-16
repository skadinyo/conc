(ns conc.alpha
  (require [clojure.core.reducers :as r]
           [conc.core :as m]))

;;problem 1 (lim 10.000.000)

(defn eul-1-1
  [lim]
  (reduce +'
          (filter #(or (= 0 (rem % 3))
                       (= 0 (rem % 5)))
                  (range 1 lim))))

;;average 550 ms

(defn eul-1-2
  "using atom"
  [lim]
  (let [res (atom 0)]
    (do
      (doseq [i (range 1 lim)
              :when (or (= 0 (rem i 3))
                        (= 0 (rem i 5)))]
        (swap! res +' i))
      @res)))

;;average 450 ms

;;problem 2

(defn lazy-fib
  ([] (lazy-fib 1 2))
  ([a b] (cons a (lazy-seq (lazy-fib b (+' a b))))))

(defn eul-2-1
  [lim]
  (->> (lazy-fib)
       (take-while #(< % lim))
       (filter even?)
       (reduce +')))

;;problem 3

(defn eul-3-1
  [x]
  (let [lim (int (Math/sqrt x))]
    (last (filter #(= 0 (rem x %)) (m/primes-to lim)))))

;;average 60 ms

;;problem 4



