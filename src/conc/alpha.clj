(ns conc.alpha
  (require [clojure.core.reducers :as r]
           [conc.core :as m]))

;;eul 97

(defn eul97
  []
  (loop [i 2 c 1]
    (if (= c 7830457)
      (rem (+ (* 28433 i) 1) 10000000000)
      (recur (rem (*' i 2) 10000000000)
             (inc c)))))

;;eul57

(defn count-num
  [n]
  (loop [c 1]
    (let [d (reduce *' (repeat c 10))]
      (if (> d n)
        c
        (recur (inc c))))))

(defn eul57-1
  []
  (count (filter (fn [[a b]]
                   (> (count-num a) (count-num b)))
                 (take 1000 (iterate (fn [[n d]]
                                       [(+' n (*' 2 d))
                                        (+' n d)])
                                     [3 2])))))





;;

(defn eul173
  [n]
  (let [[e1 e2] (loop [i 2 j 4]
                  (let [ij (- (* j j) (* i i))]
                    (if (> ij n)
                      [i j]
                      (recur (inc i)
                             (inc j)))))
        [o1 o2] (loop [i 1 j 3]
                  (let [ij (- (* j j) (* i i))]
                    (if (> ij n)
                      [i j]
                      (recur (inc i)
                             (inc j)))))
        even (for [i (range 2 (inc e2) 2)
                   j (range 2 (inc e1) 2)
                   :let [ij (- (* i i) (* j j))]
                   :when (and (> i j)
                              (<= ij n)
                              (> ij 4))]
               ij)
        odd (for [i (range 1 (inc o1) 2)
                  j (range 1 (inc o2) 2)
                  :let [ij (- (* i i) (* j j))]
                  :when (and (> i j)
                             (<= ij n)
                             (> ij 4))]
              ij)]
    (+ (count even) (count odd))))

(defn eul173-2
  [lim]
  (let [tiles (quot lim 4)
        l (int (Math/sqrt tiles))]
    (do
      (println tiles)
      (println l)
      (println (range 1 (inc l)))
      (reduce + (for [i (range 1 (inc l))]
                  (do
                    (println (- (quot tiles i) i))
                    (- (quot tiles i) i)))))))

;;

(defn bt [f & ar]
  (loop [i (apply f ar)]
    (if (fn? i)
      (recur (i))
      i)))

(defn bb
  [st]
  (loop [s (filter #{\[ \] \{ \} \( \)} st)]
    (if (or (nil? s)
            (empty? s))
      true
      (if (= s "f")
        false
        (recur (let [s1 s
                     s2 (rest s1)]
                 (loop [[a & as] s1
                        [b & bs] s2
                        res []]
                   (if (or (nil? a)
                           (nil? b))
                     "f"
                     (if (#{[\[ \]]
                            [\{ \}]
                            [\( \)]} [a b])
                       (concat res bs)
                       (recur as bs (conj res a)))))))))))

(defn jo [f]
  (fn [& a]
    (reduce (fn [a b]
              (a b)) f a)))

;;problem 3

(defn prev-prime
  [n]
  (loop [i (if (even? n)
             (- n 1)
             (- n 2))]
    (if (m/prime? i)
      i
      (recur (- i 2)))))

(defn eul-3-2
  [n]
  (let [lim (prev-prime (int (inc (Math/sqrt n))))]
    (loop [i lim]
      (if (= 0 (rem n i))
        i
        (recur (prev-prime i))))))

;;problem 96

(def s1 [[0 0 3 0 2 0 6 0 0]
         [9 0 0 3 0 5 0 0 1]
         [0 0 1 8 0 6 4 0 0]
         [0 0 8 1 0 2 9 0 0]
         [7 0 0 0 0 0 0 0 8]
         [0 0 6 7 0 8 2 0 0]
         [0 0 2 6 0 9 5 0 0]
         [8 0 0 2 0 3 0 0 9]
         [0 0 5 0 1 0 3 0 0]])

(defn solve-sudoku
  [sudoku-array]
  (let [all-post (for [i (range 0 10)
                       j (range 0 10)]
                   [i j])
        check-position (fn [n]
                         (set (filter #(= n (get-in sudoku-array %))
                                      all-post)))]
    (loop [i (check-position 9)])))
;;

;;zenleague 4

(def faktorial
  (memoize (fn [n]
             (if (>= 1 n)
               1
               (* n (faktorial (dec n)))))))
(defn number-to-collumn
  [n]
  (loop [i n res []]
    (if (< i 10)
      (cons i res)
      (recur (quot i 10)
             (cons (rem i 10) res)))))

(defn league4
  [n]
  (loop [i 1 res 0]
    (if (> i n)
      res
      (recur (inc i)
             (+ res (->> i
                         number-to-collumn
                         (map faktorial)
                         (reduce +)))))))
;;problem 18

(def eul-18
  (->> (slurp "./resources/problem-18.txt")
       ((fn [coll]
          (clojure.string/split coll #"] ")))
       (map #(re-seq #"\d+" %))
       (mapv (fn [coll]
               (mapv #(Integer/parseInt %) coll)))))

(def get-path
  (memoize (fn get-path [i j]
             (if (= 14 i)
               (get-in eul-18 [i j])
               (let [root (get-in eul-18 [i j])
                     left (get-path (inc i) j)
                     right (get-path (inc i) (inc j))]
                 (if (> left right)
                   (+ root left)
                   (+ root right)))))))

(defn eul-18-1
  []
  (get-path 0 0))

;;average 53 ms

;;problem 67

(def eul-67
  (->> (slurp "./resources/problem-67.txt")
       (clojure.string/split-lines)
       (mapv (fn [coll]
               (mapv (fn [a]
                       (Integer/parseInt a))
                     (clojure.string/split coll #" "))))))

(def get-path-2
  (memoize (fn [i j]
             (if (= i 99)
               (get-in eul-67 [i j])
               (+ (get-in eul-67 [i j])
                  (max (get-path-2 (inc i) j)
                       (get-path-2 (inc i) (inc j))))))))

(defn eul-67-1
  []
  (get-path-2 0 0))

;;average 28 ms !!!!!

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
                                 p (m/count-div p i)))))]
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
                  {i (m/count-div i n)}))
           (reduce #(merge %1 %2))))))

(defn eul-12-2
  [n]
  (loop [i 3 tri 3]
    (if (>= (let [v (vals (prime-fac tri))]
              (apply + (conj v (count v))))
            n)
      tri
      (recur (inc i)
             (reduce + (range i 0 -1))))))

;;too slow


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
  (let [d1 (not (= 0 (nth coll 0)))
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
  (let [ncoll-1 [1 4 6 0]
        ncoll-2 [1 3 4 0]
        coll-1 [3 5 7 2 8 9]
        coll-2 [9 5 2 8 6 7]
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


;;defn problem 83
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
        (let [root (first (take-last 1 (drop-last 1 path)))
              next-step (path-find root set-path used)]
          (do
            (println path)
            )
          )))))

;;not done

;; problem 54

(defn eul-54
  []
  (->> (slurp "test.txt")
       (clojure.string/split-lines)
       (mapv (fn [coll]
               (->> (clojure.string/split coll #" ")
                    (partition 5)
                    (mapv vec))))))

(defn one-pair?
  [numbers]
  (let [temp (->> numbers
                  (partition-by identity)
                  (filter #(= 2 (count %))))]
    (if (= 1 (count temp))
      (ffirst temp)
      false)))

(defn two-pair?
  [numbers]
  (let [temp (->> numbers
                  (partition-by identity)
                  (filter #(= 2 (count %))))]
    (if (= 2 (count temp))
      (ffirst (take-last 1 temp))
      false)))

(defn three-of-a-kind?
  [numbers]
  (let [temp (->> numbers
                  (partition-by identity)
                  (filter #(= 3 (count %))))]
    (if (= 1 (count temp))
      (ffirst temp)
      false)))

(defn straight?
  [num-coll]
  (if (= num-coll [2 3 4 5 14])
    14
    (reduce (fn [a b]
              (if a
                (if (= (inc a) b)
                  b
                  nil)
                nil))
            num-coll)))

(defn flush?
  [syms]
  (= 1 (count syms)))

(defn full-house?
  [numbers]
  (let [temp (frequencies numbers)]
    (if (= #{2 3} (set (vals temp)))
      (first (filter #(= 3 (temp %)) (keys temp)))
      false)))

(defn four-of-a-kind?
  [numbers]
  (let [temp (first (->> numbers
                         (partition-by identity)
                         (sort-by count)
                         (take-last 1)))]
    (if (= 4 (count temp))
      (first temp)
      false)))

(defn straight-flush?
  [numbers syms]
  (if (and (straight? numbers)
           (= 1 (count syms)))
    (last numbers)))

(defn royal-straight-flush?
  [numbers syms]
  (and (= numbers [10 11 12 13 14])
       (= 1 (count syms))))

(defn cards-rank
  "[rank rank-properties numbers]"
  [card-coll]
  (let [card-sym (->> card-coll
                      (map last)
                      (map str)
                      (set))
        val-refs (zipmap (map str (conj (vec (range 2 10)) "T" "J" "Q" "K" "A"))
                         (range 2 15))
        card-number (sort (map (fn [st]
                                 (val-refs st))
                               (->> card-coll
                                    (map first)
                                    (map str))))
        temp [(vec (reverse card-number))]]
    (let [royal-straight-flush (royal-straight-flush? card-number card-sym)
          straight-flush (straight-flush? card-number card-sym)
          four-of-a-kind (four-of-a-kind? card-number)
          full-house (full-house? card-number)
          flush (flush? card-sym)
          straight (straight? card-number)
          three-of-a-kind (three-of-a-kind? card-number)
          two-pair (two-pair? card-number)
          one-pair (one-pair? card-number)]
      (vec (cond
             royal-straight-flush (concat [10 0] temp)
             straight-flush (concat [9 straight-flush] temp)
             four-of-a-kind (concat [8 four-of-a-kind temp])
             full-house (concat [7 full-house] temp)
             flush (concat [6 0] temp)
             straight (concat [5 straight] temp)
             three-of-a-kind (concat [4 three-of-a-kind] temp)
             two-pair (concat [3 two-pair] temp)
             one-pair (concat [2 one-pair] temp)
             :else (concat [1 0] temp))))))

(defn highest-card
  [[h1 & h1s] [h2 & h2s]]
  (if (and (nil? h1)
           (nil? h2))
    "p1"
    (cond
      (> h1 h2) "p1"
      (> h2 h1) "p2"
      :else (highest-card h1s h2s))))

(defn eul-54-1
  []
  (let [cards (eul-54)
        ranks (mapv (fn [coll]
                      (mapv cards-rank coll))
                    cards)
        winner (fn [[[f1 p1 h1] [f2 p2 h2]]]
                 (cond
                   (> f1 f2) "p1"
                   (> f2 f1) "p2"
                   (= f1 f2) (cond
                               (> p1 p2) "p1"
                               (> p2 p1) "p2"
                               :else (highest-card h1 h2))
                   :else (highest-card h1 h2)))]
    (->> ranks
         (map winner)
         frequencies)))

;;average 88 ms

;;problem 104


(defn pan-fib?
  [n]
  (if (< n 1000000000000000000)
    false
    (let [coll (->> n
                    (str)
                    (map str))
          head (set (map #(Integer/parseInt %) (take 9 coll)))
          tail (set (map #(Integer/parseInt %) (take-last 9 coll)))]
      (if (or (some #{0} head)
              (some #{0} tail))
        false
        (= 9 (count head) (count tail))))))

(defn eul-104
  []
  (loop [[a b] [1 1] c 1]
    (if (pan-fib? a)
      c
      (do
        (println c)
        (recur [b (+' a b)]
               (inc c))))))

;; lama banget 2 game dota

;;problem 89

(defn roman
  []
  (->> (slurp "roman.txt")
       (clojure.string/split-lines)))

(defn rr
  [st]
  (letfn [(roman? [ch] (cond
                         (= \I ch) 1
                         (= \V ch) 5
                         (= \X ch) 10
                         (= \L ch) 50
                         (= \C ch) 100
                         (= \D ch) 500
                         (= \M ch) 1000
                         :else 0))]
    (loop [coll (map roman? st) res 0]
      (if (empty? coll)
        res
        (if (nil? (second coll))
          (+ res (first coll))
          (if (< (first coll) (second coll))
            (recur (drop 2 coll) (+ res (- (second coll) (first coll))))
            (recur (rest coll) (+ res (first coll)))))))))

(defn num-roman [i] (loop [x i res []]
                      (cond
                        (>= x 1000) (recur (- x 1000) (conj res "M"))
                        (>= x 900) (recur (- x 900) (conj res "CM"))
                        (>= x 500) (recur (- x 500) (conj res "D"))
                        (>= x 400) (recur (- x 400) (conj res "CD"))
                        (>= x 100) (recur (- x 100) (conj res "C"))
                        (>= x 90) (recur (- x 90) (conj res "XC"))
                        (>= x 50) (recur (- x 50) (conj res "L"))
                        (>= x 40) (recur (- x 40) (conj res "XL"))
                        (>= x 10) (recur (- x 10) (conj res "X"))
                        (>= x 9) (recur (- x 9) (conj res "IX"))
                        (>= x 5) (recur (- x 5) (conj res "V"))
                        (>= x 4) (recur (- x 4) (conj res "IV"))
                        (>= x 1) (recur (- x 1) (conj res "I"))
                        :else (apply str res))))

(defn eul-89-1
  []
  (time (->> (roman)
             (map (fn [st]
                    [(rr st) st]))
             (map (fn [[n temp]]
                    [(num-roman n) temp]))
             (map (fn [[a b]]
                    (- (count b) (count a))))
             (reduce +))))

;; average 25 ms

;;problem 62

(defn eul-62-1
  [lim]
  (time (->> (map #(*' % % %) (range 1000 10000))
             (map (fn [x]
                    [x (vec (sort (m/number-collumn x)))]))
             (group-by second)
             (mapv (fn [[k v]]
                     [(mapv first v)
                      (count v)]))
             (filter (fn [[k v]]
                       (= v 5)))
             (map (fn [[k v]]
                    k))
             (flatten)
             (sort)
             (first))))

;;average 118 ms


;;problem 60

(defn join-number
  [[a b]]
  (Integer/parseInt (str a b)))

(defn eul-60
  []
  (let [p (drop 1 (m/primes-to 1000))
        primes (concat (take 1 p)
                       (drop 2 p))]
    (for [i primes
          j primes
          k primes
          :when (and (not (= i j k))
                     (let [list-perm (m/n-permutes 2 [i j k])]
                       (every? m/prime?
                               (map join-number list-perm))))]
      [i j k])))

;;not done

(->> (slurp "./resources/battery.txt")
     (clojure.string/split-lines)
     (map (fn [st]
            (let [[percentage time] (clojure.string/split st #" ")]
              [(apply str (take 2 percentage)) time]))))
