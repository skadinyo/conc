module Alpha where

import Problem
import Math
import Clojure
import Data.List
import Data.Char

eul8 n = iter problem8
  where
    iter [] = 0
    iter xs = max (product (take n xs)) (iter (tail xs))

eul76 target coins
  | target < 0 = 0
  | target <= 1 = 1
  | null coins = 0
  | otherwise = (eul76 (target - (head coins)) coins) + (eul76 target (tail coins))


squareList = scanl1 (*) [1..]
square i = squareList !! i

squareDigit 1 = False
squareDigit 89 = True
squareDigit x = squareDigit $ sum $ map (\x -> x * x) $ numberDigits x

countDigit x
  | x < 10 = 1
  | otherwise = succ $ countDigit $ div x 10
