module Solution6 where

import Problem
import Math
import Clojure
import Data.List
import Data.Char

eul52 = iter 1
  where
    iter x
      | equal $ map sort $ map numberDigits $ map (\c -> c * x) [1,2,3,4,5,6] = x
      | otherwise = iter (succ x)


eul53 lim = length $ filter (\c -> c > 1000000) $ concat $ take (succ lim) pascalTriangle

eul56 lim = maximum [sum $ numberDigits $ a^b | a <- [1..lim], b <- [1..lim]]
