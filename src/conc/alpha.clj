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
        h1   (fn [i j]
               (map #(get-in refs [i %])
                 (range j (+ j 4))))
        h2   (fn [i j]
               (map #(get-in refs [(+ i 3) %])
                 (range j (+ j 4))))
        v1   (fn [i j]
               (map #(get-in refs [% j])
                 (range i (+ i 4))))
        v2   (fn [i j]
               (map #(get-in refs [% (+ j 3)])
                 (range i (+ i 4))))
        d1   (fn [i j]
               (map #(get-in refs [%1 %2])
                 (range i (+ i 4))
                 (range j (+ j 4))))
        d2   (fn [i j]
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
  (let [primes       (m/primes-to 100000)
        lim          (last primes)
        temp         (into-array (repeat lim {}))
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

;;problem 14

(defn collatz-seq
  [n]
  (loop [i n c 1]
    (if (>= 1 i)
      c
      (if (even? i)
        (recur (/ i 2) (inc c))
        (recur (+ (* 3 i) 1) (inc c))))))

(defn eul-14-1
  [lim]
  (->> (range 2 lim)
    (map (fn [a]
           [a (collatz-seq a)]))
    (sort-by last)
    (last)))

;;average 8800 ms

;;;;problem 16

(defn eul-16-1
  [a n]
  (->> (repeat n a)
    (reduce *')
    (str)
    (seq)
    (map #(Integer/parseInt (str %)))
    (reduce +)))
;;average 2.5 ms

;;problem 17

(defn one-nineteen
  [n]
  ((zipmap (range 0 20)
     ["zero"
      "one"
      "two"
      "three"
      "four"
      "five"
      "six"
      "seven"
      "eight"
      "nine"
      "ten"
      "eleven"
      "twelve"
      "thirteen"
      "fourteen"
      "fifteen"
      "sixteen"
      "seventeen"
      "eighteen"
      "nineteen"]) n))

(defn twenty-ninety
  [n]
  ((zipmap (map #(* 10 %)
             (range 2 10))
     ["twenty"
      "thirty"
      "forty"
      "fifty"
      "sixty"
      "seventy"
      "eighty"
      "ninety"]) n))


(defn hundred
  [n]
  (str (one-nineteen (quot n 100)) "hundred"))

(defn number-string
  "1 -> one"
  [n]
  (cond
    (= 1000 n) "onethousand"
    (= 0 (rem n 100)) (hundred n)
    (<= n 19) (one-nineteen n)
    (and (< n 99)
      (= 0 (rem n 10))) (twenty-ninety n)
    (>= (quot n 100) 1) (str (hundred (* 100 (quot n 100))) "and" (number-string (rem n 100)))
    (>= (quot n 10) 2) (str (twenty-ninety (* 10 (quot n 10))) (number-string (rem n 10)))
    :else nil))

;;problem 32

(defn count-dig
  [n]
  (cond
    (<= n 9) 1
    (<= n 99) 2
    (<= n 999) 3
    (<= n 9999) 4
    (<= n 99999) 5
    (<= n 999999) 6
    (<= n 9999999) 7
    (<= n 99999999) 8
    (<= n 999999999) 9
    (<= n 9999999999) 10
    :else 11))

(defn eul-32-1
  []
  (->> (for [i (range 1 2000)
             j (range 1 50)
             :let [ij (* i j)]
             :when (= 9 (apply + (map count-dig [i j ij])))
             :let [ij-set (->> [i j ij]
                            (map str)
                            (map seq)
                            (flatten)
                            (remove #(= \0 %))
                            set)]
             :when (= 9 (count ij-set))]
         ij)
    set
    (reduce +)))

;;average 600 ms

;;problem 38

(defn eul-38-1
  []
  (->> (for [n (range 1 9999)
             :let [n2 (* 2 n)]
             :when (= 9 (+ (count-dig n) (count-dig n2)))
             :let [n-set (->> [n n2]
                           (map str)
                           (map seq)
                           (flatten)
                           set
                           (remove #(= \0 %))
                           set)]
             :when (= 9 (count n-set))]
         (Integer/parseInt (str n n2)))
    (sort)
    (last)))

;; average 167 ms

;;problem 43

(defn eul-43-cond
  [coll]
  (let [d1     (not (= 0 (nth coll 0)))
        d2d3d4 (let [d (take 3 (drop 1 coll))]
                 (= 0 (rem (->> d
                             m/coll-integer)
                        2)))
        d3d4d5 (let [d (take 3 (drop 2 coll))]
                 (= 0 (rem (->> d
                             m/coll-integer)
                        3)))]
    (and d1 d2d3d4 d3d4d5)))

(defn eul-43-1
  []
  (let [ncoll-1   [1 4 6 0]
        ncoll-2   [1 3 4 0]
        coll-1    [3 5 7 2 8 9]
        coll-2    [9 5 2 8 6 7]
        calculate (fn [ncoll coll]
                    (let [permute-ncoll (m/n-permutes 4 ncoll)]
                      (->> permute-ncoll
                        (map #(concat % coll))
                        (filter eul-43-cond)
                        (map m/coll-integer)
                        (reduce +))))]
    (+ (calculate ncoll-1 coll-1)
      (calculate ncoll-2 coll-2))))

;;average 8 ms

(defn eul-49-1
  []
  (let [primes (filter #(> % 1000)
                 (m/primes-to 10000))]
    (for [i primes
          j primes
          :when (> i j)
          :let [diff (- i j)
                j2 (+ j diff)]
          :when (and (m/prime? j2)
                  (m/permutes? [j j2]))
          :let [j3 (+ j2 diff)]
          :when (and (m/prime? j3)
                  (m/permutes? [j j2 j3]))]
      (str j j2 j3))))


;;problem 49

;;average 3426 ms

(defn path-find
  ([root set-path used]
    (path-find root (clojure.set/difference set-path used)))
  ([root set-path]
   (let [[x y] root
         maybe-path (set (map (fn [a]
                                (mapv #(- %1 %2)
                                  root
                                  a))
                           [[-1 0]
                            [1 0]
                            [0 -1]
                            [0 1]]))]
     (clojure.set/intersection maybe-path set-path))))

(defn check-path
  [coll]
  (not (nil? (reduce (fn [a b]
                       (if (nil? a)
                         nil
                          (let [maybe-path (set (map (fn [x]
                                                      (mapv #(- %1 %2)
                                                        a
                                                        x))
                                                 [[-1 0]
                                                  [1 0]
                                                  [0 -1]
                                                  [0 1]]))
                               next (some maybe-path [b])]
                           (if next
                             next
                             nil)))) coll))))

(defn eul-83
  []
  [[131 673 234 103 18]
   [201 96 342 965 150]
   [630 803 746 422 111]
   [537 699 497 121 956]
   [805 732 524 37 331]])

(defn eul-83
  []
  (->> (slurp "./resources/problem-83.txt")
    (clojure.string/split-lines)
    (mapv (fn [coll]
            (mapv #(Integer/parseInt (str %))
              (clojure.string/split coll #","))))))

(defn eul-83-1
  [n]
  ())

(defn find-path-a-b
  [set-path]
  (loop [path [[0 0] [4 4]]
         temp set-path
         used #{}]
    (if (check-path path)
      path
      (if (empty? set-path)
        []
        (let [root      (first (take-last 1 (drop-last 1 path)))
              next-step (path-find root set-path used)]
          (do
            (println path)
            )
          )))))


