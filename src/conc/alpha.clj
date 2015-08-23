(ns conc.alpha
  (require [clojure.core.reducers :as r]
           [conc.core :as m]))

(defn print-print
  [coll]
  (doseq [i coll]
    (println i)))

;;problem 108

(defn div-until-n
  [n pembagi]
  (loop [i n res 0]
    (if (= 0 (rem i pembagi))
      (recur (quot i pembagi)
             (inc res))
      [i res])))

(defn prime-sieve-
  [nlim]
  (let [lim (inc nlim)
        par (int (Math/sqrt lim))
        refs (boolean-array lim true)]
    (do
      (doseq [n (range 2 lim)
              :when (aget refs n)
              :while (< n par)]
        (doseq [i (range (+ n n) lim n)]
          (aset refs i false)))
      (for [i (range 2 lim)
            :when (aget refs i)]
        i))))

(defn seret-faktor
  [nlim]
  (let [lim (inc nlim)
        primes (m/primes-to lim)
        prime-faktor (into-array (vec (repeat lim [])))]
    (do
      (doseq [n primes]
        (loop [i n]
          (if (< i lim)
            (do
              (aset prime-faktor i (conj (aget prime-faktor i)
                                         n))
              (recur (+ i n)))
            nil)))
      (for [i (range 0 lim)]
        (aget prime-faktor i)))))

(defn sieve-factor-
  [lim]
  (let [refs (into-array (repeat lim 1))]
    (do
      (doseq [n (range 2 lim)]
        (doseq [i (range n lim n)]
          (aset refs i (inc (aget refs i)))))
      refs)))

(defn eul108
  [lim]
  (let [facs (sieve-factor- lim)
        get-i (fn [i]
                (aget facs (* i i)))]
    (loop [i 2]
      (if (> (get-i i) 1000)
        i
        (recur (inc i))))))

;;problem 85
(def trinum (reductions + (range)))

(defn e85
  [x y]
  (apply * (map (partial nth trinum) [x y])))

(defn eul85
  [lim]
  (let [refs (to-array-2d (vec (repeat lim (repeat lim 1))))]
    (do
      (doseq [i (range 0 lim)]
        (let [ni (nth trinum i)]
          (doseq [j (range 0 lim)]
            (aset refs i j ni))))
      (doseq [j (range 0 lim)]
        (let [nj (nth trinum j)]
          (doseq [i (range 0 lim)]
            (aset refs i j (* (aget refs i j) nj)))))
      (->> (for [i (range 0 lim)
                 j (range 0 lim)]
             [(* i j) [i j] (aget refs i j)])
           (sort-by last)
           (take-while #(> 2000000 (last %)))
           (reverse)
           (first)))))

;;114

(defn eul144
  [batako kosong]
  (cond
    (> batako kosong) 0
    :else (let [root (+ 1 (- kosong batako))
                n-r (range batako (inc kosong) (inc batako))]
            (+ root
               (reduce + (map #(eul144 % kosong) n-r))
               (eul144 (inc batako) kosong)))))

;;121

(defn eul-121
  [n]
  (let [tmp (loop [res [1] c 1]
              (if (> c n)
                res
                (recur (let [asd (flatten (map (fn [x]
                                                 [x (* x c)]) res))
                             a (first asd)
                             d (last asd)
                             s (->> asd
                                    (butlast)
                                    (rest)
                                    (partition 2)
                                    (map (partial apply +)))]
                         (vec (concat [a] s [d])))
                       (inc c))))]
    (do
      (println tmp)
      (->> (split-at (if (even? n)
                       (quot n 2)
                       (quot (inc n) 2)) tmp)
           (map (partial apply +))
           (reverse)
           (apply /)
           (inc)
           (int)))))
;;

(def t [0 10 0.0])

(defn next-count
  [i]
  (cond
    (= 20.0 i) [0.0]
    (= 20 i) [0 0.0]
    (= 10.0 i) [0.0 20.0]
    (= 10 i) [0 0.0 20]
    (= 0.0 i) [0.0 10.0]
    :else [0 0.0 10]))

(def n-c (memoize next-count))

(defn eul-191
  [n awal]
  (loop [i awal c 1]
    (do
      (println c)
      (if (= c n)
        (count i)
        (recur (flatten (map n-c i))
               (inc c))))))
;;problem 95

(defn sieve-factor
  [lim]
  (let [factor-array (int-array (repeat (inc lim) 1))]
    (do
      (loop [i 2]
        (if (> i lim)
          nil
          (do
            (loop [j (+ i i)]
              (if (> j lim)
                nil
                (do
                  (aset factor-array j (+ i (aget factor-array j)))
                  (recur (+ j i)))))
            (recur (inc i))))))
    (vec factor-array)))

;;problem 32

(def c [200 100 50 20 10 5 2 1])

(defn eul-32
  ([] (eul-32 200 c))
  ([n coin]
   (cond
     (< n 0) 0
     (<= n 1) 1
     (empty? coin) 0
     :else (+ (eul-32 (- n (first coin)) coin)
              (eul-32 n (rest coin))))))

(def e32 (memoize eul-32))

;;problem 76

(defn eul-76
  [target coin]
  (cond
    (< target 0) 0
    (<= target 1) 1
    (empty? coin) 0
    :else (+' (eul-76 (- target (first coin)) (filter #(<= % target) coin))
              (eul-76 target (rest coin)))))

(def e76 (memoize eul-76))


;;problem 77

(def p [2 3 5 7])

(defn eul-77
  ([] (eul-77 10 p))
  ([n coin]
   (cond
     (= n 0) 1
     (< n 2) 0
     (empty? coin) 0
     :else (+ (eul-77 (- n (first coin)) coin)
              (eul-77 n (rest coin))))))

(def e77 (memoize eul-77))


(defn find-sum
  [n]
  (let [p (m/primes-to n)
        r (fn r [target ps]
            (cond
              (= 0 target) 1
              (= 2 target) 1
              (< target 2) 0
              (empty? ps) 0
              :else (let [i (- n (first ps))]
                      (+ (r i (remove #(<= i %) ps))
                         (r n (remove #(<= i %) (rest ps)))))))
        z (memoize r)]
    (z n p)))

;;problem 71

(defn eul-71-1
  [x]
  (- (* 3/7 x) 1))

;;no need to give the time ;)

(defn eul-71-2
  [lim]
  ())

(defn eul-72-1
  [lim]
  (->> (for [d (range 1 (inc lim))
             n (range 1 d)
             :let [nd (/ n d)]]
         nd)
       (distinct)
       (count)))

(defn eul-73-1
  []
  (time (->> (for [d (range 1 (inc 12000))
                   n (range 1 d)
                   :let [nd (/ n d)]
                   :when (and (< nd 1/2)
                              (> nd 1/3))]
               nd)
             (distinct)
             (count))))

;; FUCKING BRUTE AND DUMB DUMB FORCE "Elapsed time: 448661.553735 msecs"

;;problem 95

(defn eul-95-1
  ([n] (eul-95-1 n 200))
  ([n lim]
   (let [fac (int-array (repeat lim 1))
         sieve-fac (fn [x]
                     (doseq [i (range (+ x x) lim x)]
                       (aset fac i (+ x (aget fac i)))))
         find-ami (fn [x]
                    (loop [i (get fac x) temp #{}]
                      (let [ne (get fac i)]
                        (if (or (temp i)
                                (> i lim))
                          (count temp)
                          (recur ne
                                 (conj temp i))))))]
     (do
       (doseq [i (range 2 (quot lim 2))]
         (do
           (println i)
           (sieve-fac i)))
       (loop [i 2 res [0 0]]
         (if (= i lim)
           res
           (let [a (find-ami i)]
             (do
               (println i a)
               (recur (inc i)
                      (if (and (> a (last res))
                               (< a 1000000))
                        [i a]
                        res))))))))))



;;problem 512

(defn odd-totient-to
  [lim]
  (let [p (m/primes-to lim)
        tot (long-array (range 0 lim))]
    (do
      (doseq [ip p]
        (do
          (aset tot ip (dec ip))
          (doseq [i (filter odd? (range (*' 2 ip) lim ip))]
            (aset tot i (int (/ (*' (aget tot i) (dec ip))
                                ip))))))
      (reduce +' (map #(aget tot %) (range 1 lim 2))))))

;;problem 187

(defn eul-187-1
  [lim]
  (let [res (atom 0)
        ps (m/primes-to (/ lim 2))
        p (m/primes-to (Math/sqrt lim))]
    (do (doseq [i p]
          (doseq [j (take-while #(> lim (* i %)) (drop-while #(< % i) ps))]
            (swap! res inc)))
        @res)))

(defn eul-187-2
  [lim]
  (let [ps (m/primes-to (/ lim 2))
        l (Math/sqrt lim)
        p (take-while #(< % l) ps)]
    (loop [[i & is] p res 0]
      (if i
        (recur is
               (+ res (count (take-while #(> lim (* i %)) (drop-while #(< % i) ps)))))
        res))))

(defn eul-187-3
  [lim]
  (let [ps (m/primes-to (/ lim 2))
        l (Math/sqrt lim)
        p (take-while #(< % l) ps)]
    (apply + (pmap #(count (->> ps
                                (drop-while (fn [x]
                                              (< x %)))
                                (take-while (fn [x]
                                              (> lim (* % x))))))
                   p))))

;;problem 204

(defn remove-primefac
  [n prime-coll]
  (reduce (fn [coll r]
            (remove #(= 0 (rem % r)) coll)) (range 1 n) prime-coll))

(def primes [2 3 5])

(def limit 100000000)

(defn p204
  [n]
  (if (> n limit)
    [0]
    (concat [n]
            (p204 (* n 2))
            (p204 (* n 3))
            (p204 (* n 5)))))

(def pp (memoize p204))

(defn eul-204-1
  [n lim]
  (let [ps (m/primes-to (inc n))
        c (fn [p]
            (take-while #(< % lim) (iterate #(* % p) p)))
        asu (map c ps)]
    (loop [[x & xs] asu res #{1}]
      (if x
        (let [temp (set (for [i x
                              j res
                              :let [ij (* i j)]
                              :when (<= ij lim)]
                          ij))]
          (recur xs
                 (clojure.set/union res temp (set x))))
        (count res)))))

;;problem 12

(defn g-tri
  []
  (map #(/ (* % (inc %)) 2) (range)))

;;problem 60

(defn concat-number
  [a b]
  (let [as (m/number-coll a)
        bs (m/number-coll b)]
    (m/coll-integer (concat as bs))))

(defn eul-60-1
  []
  (let [primes-all (m/primes-to 1000000)
        primes (take-while #(< % 10000))
        refs (set primes-all)
        p? (fn [x]
             (if (> x 1000000)
               (m/prime? x)
               (some #{x} refs)))
        pair-prime? (fn [coll]
                      (let [pair (m/n-permutes 2 coll)]
                        (loop [[p & ps] pair]
                          (if p
                            (if (p? (apply concat-number p))
                              (recur ps)
                              false)
                            true))))]
    ))

;;generate palindrome

(defn gen-palin
  []
  ())

;;problem 132

(defn n-repunit
  [n]
  (m/coll-integer (repeat n 1)))

(defn eul-132
  [n]
  (let [repunit (n-repunit n)]
    (loop [i 2 temp repunit res []]
      (if (or (= 1 temp)
              (= 49 (count res)))
        res
        (let [x (m/div-until temp i)]
          (recur (m/next-prime i)
                 x
                 (if (= x temp)
                   res
                   (conj res i))))))))

;;problem 124

(defn eul-124
  [n lim]
  (let [radicals (int-array (repeat (inc lim) 1))
        sieve-n (fn [x]
                  (doseq [i (range x (inc lim) x)]
                    (aset radicals i (* (aget radicals i) x))))]
    (do
      (doseq [i (m/primes-to (inc lim))]
        (sieve-n i))
      (vec radicals))))

;;provlem 65

(defn eul-65-1
  []
  (time (reduce +
                (m/number-coll
                  (loop [[m & ms] (drop 1
                                        (take 98
                                              (flatten
                                                (interleave (range 2 10000 2)
                                                            (repeat [1 1])))))
                         [n1 n2] [3 8]]
                    (do
                      (println n1 n2 m)
                      (if (nil? m)
                        n2
                        (recur ms
                               [n2 (+' (*' n2 m) n1)]))))))))



;;probem214 -notdone

(defn tot-chain
  [refs x]
  (if (= 2 x)
    2
    (+ 1 (tot-chain refs (refs x)))))

(defn eul-214-1
  [lim]
  (let [tot (into {} (m/totient-to lim))
        p (m/primes-to lim)
        toti (memoize tot-chain)]
    (filter #(= 25 %) (map toti (repeat tot) p))))

;;problem 357 -notdone

(defn prime-fac?
  [n]
  (let [p (m/primes-to (inc n))]
    (= 1 (->> p
              (filter #(= 0 (rem n %)))
              (reduce / n)))))

(let [ps (m/primes-to 1000)
      p-set (set ps)]
  (->> ps
       (filter #(= 3 (rem % 4)))
       (filter #(p-set (+ (/ (dec %) 2) 2)))
       (map dec)))



;; problem 164

(defn c164
  ([] (map c164 (repeat 1) (range 0 10)))
  ([p n] (if (= p 10)
           ())))

;;problem 345 -notdone

(def mat
  (->> "  7  53 183 439 863 497 383 563  79 973 287  63 343 169 583
627 343 773 959 943 767 473 103 699 303 957 703 583 639 913
447 283 463  29  23 487 463 993 119 883 327 493 423 159 743
217 623   3 399 853 407 103 983  89 463 290 516 212 462 350
960 376 682 962 300 780 486 502 912 800 250 346 172 812 350
870 456 192 162 593 473 915  45 989 873 823 965 425 329 803
973 965 905 919 133 673 665 235 509 613 673 815 165 992 326
322 148 972 962 286 255 941 541 265 323 925 281 601  95 973
445 721  11 525 473  65 511 164 138 672  18 428 154 448 848
414 456 310 312 798 104 566 520 302 248 694 976 430 392 198
184 829 373 181 631 101 969 613 840 740 778 458 284 760 390
821 461 843 513  17 901 711 993 293 157 274  94 192 156 574
 34 124   4 878 450 476 712 914 838 669 875 299 823 329 699
815 559 813 459 522 788 168 586 966 232 308 833 251 631 107
813 883 451 509 615  77 281 613 459 205 380 274 302  35 805"
       (clojure.string/split-lines)
       (map #(clojure.string/split % #" "))
       (map #(remove (fn [st] (= "" st)) %))
       (mapv (fn [coll] (mapv #(Integer/parseInt %) coll)))))

(def mat
  (->> " 7  53 183 439 863
497 383 563  79 973
287  63 343 169 583
627 343 773 959 943
767 473 103 699 303"
       (clojure.string/split-lines)
       (map #(clojure.string/split % #" "))
       (map #(remove (fn [st] (= "" st)) %))
       (mapv (fn [coll] (mapv #(Integer/parseInt %) coll))))
  )

(defn call
  [states [ir jr]]
  (let [is (for [i (range 0 5)
                 :when (nil? (states i))]
             [i (inc jr)])]
    is))

(defn eul-345-1
  []
  (let []))

;;problem 60 -notdone

(defn eul-60-1
  [n]
  (let [pps (m/primes-to n)
        ps (take-while #(< % n) pps)
        sps (set ps)]
    (set (for [i ps
               j ps
               :let [ij (Integer/parseInt (str i j))
                     ji (Integer/parseInt (str j i))]
               :when (and (not (= i j))
                          (and (if (> ij n)
                                 (m/prime? ij)
                                 (sps ij))
                               (if (> ji n)
                                 (m/prime? ji)
                                 (sps ji))))]
           #{i j}))))

;;problem 66 -notdone

(defn sq
  [n]
  (* n n))

(defn psq?
  [n]
  (let [sqrt (Math/sqrt n)]
    (== sqrt (int sqrt))))

(defn eul-66
  [lim]
  (let [f (fn [i]
            [i (set (for [x (range 1 lim)
                          y (range 1 lim)
                          :while (= (* i (sq y)) (- (sq x) 1))]
                      x))])]
    (->> (range 1 1000)
         (remove psq?)
         (map f)
         (filter #(not (empty? (last %)))))))

;;problem 61 -notdone

(defn gen-3
  []
  (let [next-i (fn [[i n]]
                 [(/ (* n (+ n 1)) 2)
                  (inc n)])]
    (iterate next-i [1 2])))

(defn gen-4
  []
  (let [next-i (fn [[i n]]
                 1 [(* n n)
                    (inc n)])]
    (iterate next-i [1 2])))

(defn gen-5
  []
  (let [next-i (fn [[i n]]
                 [(/ (* n (+ (* 3 n) -1)) 2)
                  (inc n)])]
    (iterate next-i [1 2])))

(defn gen-6
  []
  (let [next-i (fn [[i n]]
                 [(* n (+ (* 2 n) -1))
                  (inc n)])]
    (iterate next-i [1 2])))

(defn gen-7
  []
  (let [next-i (fn [[i n]]
                 [(/ (* n (+ (* 5 n) -3)) 2)
                  (inc n)])]
    (iterate next-i [1 2])))

(defn gen-8
  []
  (let [next-i (fn [[i n]]
                 [(* n (+ (* 3 n) -2))
                  (inc n)])]
    (iterate next-i [1 2])))

(defn take-4
  [f]
  (map first (drop-while #(< (first %) 999) (take-while #(< (first %) 10000) (f)))))

(defn make-set
  [n]
  (let [f (quot n 100)
        b (rem n 100)
        two (fn [i]
              [(quot i 10) (rem i 10)])]
    {(set (two f)) (set (two b))}))

(defn eul-61-1
  []
  (let [gen (fn [f]
              (->> f
                   (take-4)
                   (map make-set)))
        [i3 i4 i5 i6 i7 i8] (->> [gen-3 gen-4 gen-5 gen-6 gen-7 gen-7]
                                 (map gen))]
    (filter #(% (ffirst i3)) i4)))


;;generate palindrome from n
(def num2seq (fn [n]
               (loop [i n res []]
                 (if (< i 10)
                   (cons i res)
                   (recur (quot i 10)
                          (cons (rem i 10) res))))))


;;---

;;problem 70

(defn perm?
  [a b]
  (let [as (sort (num2seq a))
        bs (sort (num2seq b))]
    (and (= (count as)
            (count bs))
         (= as
            bs))))

(defn eul-70-1
  []
  (time (->> (m/totient-to 10000000)
             (filter #(apply perm? %))
             (map (fn [[a b]]
                    [a b (/ a b)]))
             (sort-by last)
             (take 2)
             (last)
             (first))))

;;---

(def palin? (fn [n]
              (let [s (num2seq n)]
                (= s (reverse s)))))

(defn gen-palin
  [num]
  (let [num2seq (fn [n]
                  (loop [i n res []]
                    (if (< i 10)
                      (cons i res)
                      (recur (quot i 10)
                             (cons (rem i 10) res)))))

        palin? (fn [n]
                 (let [s (num2seq n)]
                   (= s (reverse s))))
        count-num (fn [n]
                    (loop [i 1 j 10]
                      (if (< n j)
                        i
                        (recur (inc i)
                               (* 10 j)))))
        mid (fn [n]
              (if (< n 10)
                n
                (let [s (num2seq n)
                      c (count s)]
                  (if (even? c)
                    (take 2 (drop (dec (/ c 2)) s))
                    (vector (nth s (+ 0.5 (/ c 2))))))))
        seq2num (fn [coll]
                  (reduce + (map * (reverse coll) (iterate #(* 10 %) 1))))
        inc-add (fn [n]
                  (let [c3 (let [x (mid n)]
                             (or (= [9] x)
                                 (= [9 9] x)))
                        c2 (every? #{9} (rest (butlast (num2seq n))))
                        c1 (every? #{9} (num2seq n))
                        c (count-num n)
                        adder (let [refs (reduce concat
                                                 (take (/ c 2)
                                                       (iterate (fn [coll]
                                                                  (mapv #(*' 10 %) coll))
                                                                [1 11])))
                                    temp (nth refs (dec c))]
                                (cond
                                  c1 2
                                  c2 (quot temp (cond
                                                  (< c 6) 10
                                                  (< c 8) 100
                                                  (< c 10) 1000
                                                  (< c 12) 1000))
                                  c3 (quot temp 10)
                                  :else temp))]
                    (+ n adder)))
        find-start-palin (fn find-start-palin [n]
                           (if (palin? n)
                             n (if (< n 10)
                                 n
                                 (let [s (num2seq n)
                                       c (count s)
                                       np (if (even? c)
                                            (let [half (take (/ c 2) s)]
                                              (seq2num (concat half (reverse half))))
                                            (let [half (take (+ 0.5 (/ c 2)) s)]
                                              (seq2num (concat half (drop 1 (reverse half))))))]
                                   (if (> n np)
                                     (let [y (first (drop-while #(> n %) (iterate inc-add np)))]
                                       (if (palin? y)
                                         y
                                         (find-start-palin y)))
                                     np)))))
        next-palin (fn [z]
                     (let [c (every? #{9} (num2seq z))
                           ne (inc-add z)]
                       (if c
                         (+ z 2)
                         (if (palin? ne)
                           ne
                           (find-start-palin ne)))))]
    ;;(find-start-palin num)
    (iterate next-palin (find-start-palin num))
    ))

(defn palindrome
  [num]
  (let [num2seq (fn [n]
                  (loop [i n res []]
                    (if (< i 10)
                      (cons i res)
                      (recur (quot i 10)
                             (cons (rem i 10) res)))))
        palin? (fn [n]
                 (let [s (num2seq n)]
                   (= s (reverse s))))
        count-num (fn [n]
                    (loop [i 1 j 10]
                      (if (< n j)
                        i
                        (recur (inc i)
                               (* 10 j)))))
        mid (fn [x]
              (let [i (count-num x)
                    is (num2seq x)]
                (if (odd? i)
                  (nth is (int (/ i 2)))
                  (if (= [9 9] (take 2 (drop (dec (/ i 2)) is)))
                    9))))
        seq2num (fn [coll]
                  (reduce + (map * (reverse coll) (iterate #(* 10 %) 1))))
        palin-adder (fn [n]
                      (let [c (count-num n)
                            ns (num2seq n)
                            divid ()
                            refs (reduce concat
                                         (take (/ c 2)
                                               (iterate (fn [coll]
                                                          (mapv #(*' 10 %) coll))
                                                        [1 11])))
                            temp (nth refs (dec c))]
                        (if (every? #{9} ns)
                          (+ n 2)
                          (let [ns (rest (butlast ns))
                                ms (mid n)
                                os [(first ns) (last ns)]]
                            (if (and (= 9 ms)
                                     (= 1 (count (set os))))
                              (+ n (nth refs (if (odd? c)
                                               (if (< c 5)
                                                 (- c 2)
                                                 1)
                                               (- c 3))))
                              (+ n temp))))))]
    (drop-while #(> num %)) (iterate palin-adder 0)))


;;eul205

(defn d4
  []
  (inc (rand-int 4)))

(defn d6
  []
  (inc (rand-int 6)))

(defn eul205-1
  [lim]
  (let [c (ref 0)]
    (do
      (pmap (fn [a]
              (let [peter (apply + (repeatedly 9 d4))
                    colin (apply + (repeatedly 6 d6))]
                (if (> peter colin)
                  (dosync
                    (commute c inc)))))
            (repeat lim 1))
      (/ @c lim 1.0000000000))))

(defn eul205-2
  [lim]
  (loop [c 0 cou 0]
    (if (= c lim)
      (/ cou lim 1.00000000000000)
      (recur (inc c)
             (if (> (apply + (repeatedly 9 d4))
                    (apply + (repeatedly 6 d6)))
               (inc cou)
               cou)))))

(defn eul205-1
  [lim]
  (loop [c 0 res 0]
    (let [pete (reduce + (repeatedly 9 d4))
          colin (reduce + (repeatedly 6 d6))]
      (if (= lim c)
        (/ res c 1.00000000000)
        (recur (inc c)
               (if (> pete colin)
                 (inc res)
                 res))))))





;;problem 173 -notdone

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
