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

(defn count-div
  [a b]
  (loop [temp b res 0]
    (if (not (= 0 (rem temp a)))
      res
      (recur (quot temp a) (inc res)))))

(defn eul-5-1
  [lim]
  (let [ns (range 1 (inc lim))]
    (->> (for [i (m/primes-to (inc lim))]
           [i (->> ns
                   (filter #(= 0 (rem % i)))
                   (map #(count-div i %))
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

;;problem 11

(defn eul-11
  []
  (->> (slurp "./resources/problem-11.txt")
       (clojure.string/split-lines)
       (mapv #(mapv (fn [a]
                      (Integer/parseInt a))
                    (map str
                         (clojure.string/split % #" "))))))

(defn eul-11-1
  []
  (let [refs (eul-11)
        h1 (fn [i j]
             (map #(get-in refs [i %])
                  (range j (+ j 4))))
        h2 (fn [i j]
             (map #(get-in refs [(+ i 3) %])
                  (range j (+ j 4))))
        v1 (fn [i j]
             (map #(get-in refs [% j])
                  (range i (+ i 4))))
        v2 (fn [i j]
             (map #(get-in refs [% (+ j 3)])
                  (range i (+ i 4))))
        d1 (fn [i j]
             (map #(get-in refs [%1 %2])
                  (range i (+ i 4))
                  (range j (+ j 4))))
        d2 (fn [i j]
             (map #(get-in refs [%1 %2])
                  (range (+ 3 i) (dec i) -1)
                  (range j (+ j 4))))
        call (fn [i j]
               (map (fn [f]
                      (f i j))
                    [h1 h2 v1 v2 d1 d2]))]
    (->> (for [i (range 0 (- (count refs) 4))
               j (range 0 (- (count refs) 4))]
           (map #(reduce * %)
                (call i j)))
         (flatten)
         (apply max))))

;;average 16 ms

;;problem 12

(defn gen-tri
  "generate triangle number"
  ([] (gen-tri 1))
  ([x]
   (cons (reduce + (range 1 (inc x))) (lazy-seq (gen-tri (inc x))))))

(defn eul-12-1
  [n]
  (let [primes (m/primes-to 100000)
        lim (last primes)
        temp (into-array (repeat lim {}))
        update-array (fn [p]
                       (doseq [i (range p lim p)]
                         (aset temp i
                               (assoc
                                 (aget temp i)
                                 p (count-div p i)))))]
    (do
      (doseq [p primes]
        (update-array p))
      (let [refs (int-array (map #(let [v (vals %)]
                                   (if (nil? v)
                                     0
                                     (apply + (conj v (count v)))))
                                 temp))]
        (loop [[x & xs] (take-while #(< % lim) (gen-tri))]
          (if (nil? x)
            false
            (if (>= (aget refs x) n)
              x
              (recur xs))))))))

;;sieve not done

(defn prime-fac
  [n]
  (let [p (m/primes-to (inc n))]
    (if (empty? p)
      {}
      (->> p
           (filter #(zero? (rem n %)))
           (map (fn [i]
                  {i (count-div i n)}))
           (reduce #(merge %1 %2))))))

(defn eul-12-2
  [n]
  (loop [i 3 tri 3]
    (do
      (println i " " tri " " (prime-fac tri))
      (if (>= (let [v (vals (prime-fac tri))]
                (apply + (conj v (count v))))
              n)
        tri
        (recur (inc i)
               (reduce + (range i 0 -1)))
        ))))

;;too slow

;;problem 13

(defn eul-13-1
  []
  (->> (slurp "./resources/problem-13.txt")
       (clojure.string/split-lines)
       (map (fn [st]
              (apply str (concat []
                                 [(first st)]
                                 ["."]
                                 (drop 1 st)))))
       (map #(Double/parseDouble %))
       (reduce +)
       (str)
       (seq)
       (take 11)
       (filter #(not (#{\.} %)))
       (apply str)))

;;average 3 ms