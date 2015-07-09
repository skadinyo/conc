(ns conc.problem-set-2-11-20
  (require [clojure.core.reducers :as r]
           [conc.core :as m]))

;;still not done 12 15 18 19 20

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

;;reserved for problem 12



;;

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
  []
  (->> (range 2 1000000)
    (map (fn [a]
           [a (collatz-seq a)]))
    (sort-by last)
    (last)))

;;average 8800 ms

;;reserved for problem 15

;;

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

(defn eul-17-1
  []
  (->> (range 1 1001)
    (map number-string)
    (map seq)
    (flatten)
    (count)))

;;average 35 ms

;;reserved for problem 18

;;

;;reserved for problem 19

;;

;;reserved for problem 20

;;
