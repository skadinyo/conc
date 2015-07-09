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

(defn primes-tox
  "Computes lazy sequence of prime numbers up to a given number using sieve of Eratosthenes"
  [n]
  (let [root (-> n Math/sqrt long),
        rootndx (long (/ (- root 3) 2)),
        ndx (max (long (/ (- n 3) 2)) 0),
        lmt (quot ndx 64),
        cmpsts (long-array (inc lmt)),
        cullp (fn [i]
                (let [p (long (+ i i 3))]
                  (loop [i (bit-shift-right (- (* p p) 3) 1)]
                    (if (<= i ndx)
                      (do (let [w (bit-shift-right i 6)]
                            (aset cmpsts w (bit-or (aget cmpsts w)
                                                   (bit-shift-left 1 (bit-and i 63)))))
                          (recur (+ i p))))))),
        cull (fn [] (do (aset cmpsts lmt (bit-or (aget cmpsts lmt)
                                                 (bit-shift-left -2 (bit-and ndx 63))))
                        (loop [i 0]
                          (when (<= i rootndx)
                            (when (zero? (bit-and (aget cmpsts (bit-shift-right i 6))
                                                  (bit-shift-left 1 (bit-and i 63))))
                              (cullp i))
                            (recur (inc i))))))
        numprms (fn []
                  (let [w (dec (alength cmpsts))]           ;; fast results count bit counter
                    (loop [i 0, cnt (bit-shift-left (alength cmpsts) 6)]
                      (if (> i w) cnt
                                  (recur (inc i)
                                         (- cnt (java.lang.Long/bitCount (aget cmpsts i))))))))]
    (if (< n 2) nil
                (cons 2 (if (< n 3) nil
                                    (do (cull)
                                        (deftype OPSeq [^long i ^longs cmpsa ^long cnt ^long tcnt] ;; for arrays maybe need to embed the array so that it doesn't get garbage collected???
                                          clojure.lang.ISeq
                                          (first [_] (if (nil? cmpsa) nil (+ i i 3)))
                                          (next [_] (let [ncnt (inc cnt)] (if (>= ncnt tcnt) nil
                                                                                             (OPSeq.
                                                                                               (loop [j (inc i)]
                                                                                                 (let [p? (zero? (bit-and (aget cmpsa (bit-shift-right j 6))
                                                                                                                          (bit-shift-left 1 (bit-and j 63))))]
                                                                                                   (if p? j (recur (inc j)))))
                                                                                               cmpsa ncnt tcnt))))
                                          (more [this] (let [ncnt (inc cnt)] (if (>= ncnt tcnt) (OPSeq. 0 nil tcnt tcnt)
                                                                                                (.next this))))
                                          (cons [this o] (clojure.core/cons o this))
                                          (empty [_] (if (= cnt tcnt) nil (OPSeq. 0 nil tcnt tcnt)))
                                          (equiv [this o] (if (or (not= (type this) (type o))
                                                                  (not= cnt (.cnt ^OPSeq o)) (not= tcnt (.tcnt ^OPSeq o))
                                                                  (not= i (.i ^OPSeq o))) false true))
                                          clojure.lang.Counted
                                          (count [_] (- tcnt cnt))
                                          clojure.lang.Seqable
                                          (clojure.lang.Seqable/seq [this] (if (= cnt tcnt) nil this))
                                          clojure.lang.IReduce
                                          (reduce [_ f v] (let [c (- tcnt cnt)]
                                                            (if (<= c 0) nil
                                                                         (loop [ci i, n c, rslt v]
                                                                           (if (zero? (bit-and (aget cmpsa (bit-shift-right ci 6))
                                                                                               (bit-shift-left 1 (bit-and ci 63))))
                                                                             (let [rrslt (f rslt (+ ci ci 3)),
                                                                                   rdcd (reduced? rrslt),
                                                                                   nrslt (if rdcd @rrslt rrslt)]
                                                                               (if (or (<= n 1) rdcd) nrslt
                                                                                                      (recur (inc ci) (dec n) nrslt)))
                                                                             (recur (inc ci) n rslt))))))
                                          (reduce [this f] (if (nil? i) (f) (if (= (.count this) 1) (+ i i 3)
                                                                                                    (.reduce ^clojure.lang.IReduce (.next this) f (+ i i 3)))))
                                          clojure.lang.Sequential
                                          Object
                                          (toString [this] (if (= cnt tcnt) "()"
                                                                            (.toString (seq (map identity this))))))
                                        (->OPSeq 0 cmpsts 0 (numprms))))))))

(defn all-combination
  [coll]
  (let [init (set (map set (partition 1 coll)))
        lim (count coll)]
    (loop [])))

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
    (if (not (= 0 (rem temp a)))
      res
      (recur (quot temp a) (inc res)))))

(defn psquare?
  [n]
  (let [sn (Math/sqrt n)]
    (== sn (int sn))))