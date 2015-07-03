(ns conc.unsorted
  (:require [conc.core :as m]))

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


;;problem 80

(defn nsqrt
  [n]
  (let [as (*' 5 n)
        bs 5
        r (fn [[a b]]
            (if (>= a b)
              [(-' a b) (+' b 10)]
              [(*' a 100) (let [i (quot b 10)
                                j (rem b 10)]
                            (+' (*' 100 i)
                                j))]))]
    (iterate r [as bs])))

(defn count-sqrt
  [n]
  (let [limit (reduce *' (repeat 101 10))
        n (take 1 (drop-while #(< (last %) limit) (nsqrt n)))]
    (->> (last (last n))
         (m/number-coll)
         (take 100)
         (reduce +))))

(defn eul-80
  [lim]
  (let [n (remove m/psquare? (range 1 (inc lim)))]
    (reduce + (map count-sqrt n))))



;;eul 97

(defn eul97
  []
  (loop [i 2 c 1]
    (if (= c 7830457)
      (rem (+ (* 28433 i) 1) 10000000000)
      (recur (rem (*' i 2) 10000000000)
             (inc c)))))

;;provlem 206

(def init206
  [1 0 2 0 3 0 4 0 5 0 6 0 7 0 8 0 9])

(defn masuk
  [a b]
  [a b])

(defn interleave???
  [coll]
  (conj (vec (mapcat masuk (range 1 9) coll)) 9))

(defn wrap-it!
  [n]
  (m/coll-integer (interleave??? (m/number-coll n))))

(defn eul-296-1
  []
  (loop [i 99999999]
    (if (m/psquare? (wrap-it! i))
      (Math/sqrt (wrap-it! i))
      (recur (dec i)))))

;;problem 315

(defn masuk [a b] [a b])

(defn clock-seq
  [n]
  (let [ns (vec (m/number-coll n))]
    (if (< n 10)
      [[n]]
      (loop [coll ns
             res []]
        (let [x (reduce + coll)]
          (if (< x 10)
            (conj res coll [x])
            (recur (vec (m/number-coll x))
                   (conj res coll))))))))

(def nil-num
  (zipmap (range 0 10) [6 2 5 5 4 5 6 4 7 6]))

(def number-clock
  {0 [1 1 1 1 1 1 0]
   1 [0 1 1 0 0 0 0]
   2 [1 1 0 1 1 0 1]
   3 [1 1 1 1 0 0 1]
   4 [0 1 1 0 0 1 1]
   5 [1 0 1 1 0 1 1]
   6 [1 0 1 1 1 1 1]
   7 [1 1 1 0 0 1 0]
   8 [1 1 1 1 1 1 1]
   9 [1 1 1 1 0 1 1]})

(defn sam-seq
  [n]
  (concat [nil] (mapcat masuk
                        (clock-seq n)
                        (repeat nil))))

(defn calculate-trans
  [coll]
  (reduce + (map nil-num coll)))

(defn calculate-sam
  [n]
  (let [ns (sam-seq n)]
    (*' 2 (reduce +' (map calculate-trans ns)))))

;;63424722

(defn calculate-diff
  [start end]
  (let [start-coll (map number-clock start)
        end-coll (map number-clock end)]
    (apply + (map (fn [xcoll ycoll]
                    (apply + (map (fn [a b]
                                    (cond
                                      (= [1 1] [a b]) 0
                                      (= [0 0] [a b]) 0
                                      :else 1)) xcoll ycoll)))
                  start-coll end-coll))))

(defn calculate-max
  [n]
  (let [ns (clock-seq n)]
    (+ (calculate-trans (first ns))
       (calculate-trans (last ns))
       (loop [[x1 x2 & xs] ns res 0]
         (let [c1 (count x1)
               c2 (count x2)
               cd (- c1 c2)
               dx1 (drop cd x1)
               pala (calculate-trans (take cd x1))]
           (if (nil? x2)
             res
             (recur (concat [x2] xs)
                    (+ res pala (calculate-diff dx1 x2)))))))))

