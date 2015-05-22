(ns conc.problem-set-1-1-10
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
  ([] (lazy-fib 1 1))
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

;;naive implementation

(defn palin?
  [x]
  (let [st (str x)]
    (= st (apply str (reverse st)))))

(defn eul-4-1
  [digit]
  (let [lower-lim (reduce *' (repeat (dec digit) 10))
        upper-lim (reduce *' (repeat digit 10))]
    (->> (for [i (range lower-lim upper-lim)
               j (range lower-lim upper-lim)
               :when (> j i)
               :when (or (= 0 (rem i 11))
                       (= 0 (rem j 11)))
               :let [x (* i j)]
               :when (palin? x)]
           x)
      (apply max))))

[913 993]

;;average 85 ms

(defn eul-4-2
  [digit]
  (let [lim (dec (reduce *' (repeat digit 10)))]
    (loop [a lim b lim]
      (let [x (*' a b)]
        (if (palin? x)
          (print a b x)
          (if (and (= 6 (count (seq (str x))))
                (> (* a b) (* a (dec a))))
            (recur a (dec b))
            (recur (dec a) lim)))))))

;;still not done

;;problem 5

(defn eul-5-1
  [lim]
  (let [ns (range 1 (inc lim))]
    (->> (for [i (m/primes-to (inc lim))]
           [i (->> ns
                (filter #(= 0 (rem % i)))
                (map #(m/count-div i %))
                (apply max))])
      (map #(let [[a b] %]
             (reduce * (repeat b a))))
      (reduce *'))))

;;average 0.2 ms

;;problem 6

(defn eul-6-1
  [lim]
  (let [sum (*' lim (/ (inc lim) 2))]
    (- (* sum sum)
      (* (+ (* 2 lim) 1)
        (inc lim)
        (/ lim 6)))))


;;problem 7

;;next-prime

(defn eul-7-1
  [n]
  (cond
    (= 1 n) 2
    :else (loop [i 3 res 2]
            (if (= n res)
              i
              (let [next-i (+' i 2)]
                (if (m/prime? next-i)
                  (recur next-i (inc res))
                  (recur next-i res)))))))

;;problem 8

(defn eul-8
  []
  (->> (slurp "resources/problem-8.txt")
    clojure.string/split-lines
    (map seq)
    flatten
    (mapv #(Integer/parseInt (str %)))))

(defn eul-8-1
  [n]
  (let [refs (eul-8)]
    (->> (for [i (range 0 (- (count refs) n))]
           (->> refs
             (drop i)
             (take n)))
      (filter #(nil? (some #{0} %)))
      (map #(reduce *' %))
      (apply max))))

;;average 11-20 ms

;;problem 9

(defn eul-9-1®
  [n]
  (for [b (range 4 n)
        c (range 3 n)
        :let [a (- n b c)]
        :when (and (> a b c)
                (= (* a a)
                  (+ (* b b)
                    (* c c))))]
    (* a b c)))

;;problem 10

(defn eul-10-1
  [lim]
  (reduce +' (m/primes-to lim)))

;;avg 100 ms
