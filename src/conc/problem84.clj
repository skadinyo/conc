(ns conc.problem84)

(def board [:GO :A1 :CC1 :A2 :T1 :R1 :B1 :CH1 :B2 :B3
            :JAIL :C1 :U1 :C2 :C3 :R2 :D1 :CC2 :D2 :D3
            :FP :E1 :CH2 :E2 :E3 :R3 :F1 :F2 :U2 :F3
            :G2J :G1 :G2 :CC3 :G3 :R4 :CH3 :H1 :T2 :H2])

(def number-board
  (zipmap (range) board))

(def board-number
  (zipmap board (range)))

(def cc
  (atom (shuffle (take 16 (concat [:GO :JAIL] (cycle [0]))))))

(defn take-cc
  []
  (first @cc))

(defn update-cc
  []
  (swap! cc (fn [old]
              (let [[x & xs] old]
                (vec (concat xs [x]))))))

(def ch
  (atom (shuffle [:GO :JAIL :C1 :E3 :H2 :R1 :RAILWAY :RAILWAY :UTILITY -3 0 0 0 0 0 0])))

(defn take-ch
  []
  (first @ch))

(defn update-cc
  []
  (swap! ch (fn [old]
              (let [[x & xs] old]
                (vec (concat xs [x]))))))

(defn move-to
  [init d]
  (number-board (rem (+ (number-board init d)) 40)))

(defn dice
  [n]
  (inc (rand-int n)))






