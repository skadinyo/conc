module Solution2 where

import Problem
import Math
import Clojure
import Data.List
import Data.Char

eul12 fac = iter triangleNumber
  where
    iter (x:xs)
      | (countFactor x) >= fac = x
      | otherwise = iter xs

----------

countCollatz x
  | x == 1 = 1
  | even x = succ (countCollatz (div x 2))
  | otherwise = succ (countCollatz (succ (3 * x)))

----------

eul14 lim = maxBy last (map (\x -> [x,(countCollatz x)]) [1..(pred lim)])

eul15 l = pascalTriangle !! (l+l) !! l

eul16 n = sumDigits $ pow2 n

eul20 n = sumDigits $ factorial n
