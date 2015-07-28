module Solution5 where

import Problem
import Math
import Clojure
import Data.List
import Data.Char

----------
nameNumber :: [Char] -> Integer
nameNumber x = toInteger $ sum $ map (\i -> i - 64) $ map ord x

triangleNumberRefs = take 100 triangleNumber

eul42 = length $ filter (\c -> elem c triangleNumberRefs) $ map nameNumber problem42
----------

eul47 n = iter 1 0
  where
    iter x temp
      | temp == n = x - n
      | n == (countPrimeFactors x) = iter (succ x) (succ temp)
      | otherwise = iter (succ x) 0

eul48 = rem (sum [ rem (a^a) (10^11) | a <- [1..1000]]) (10^10)

eul49 = iter (genPrimeBetween 1000 9999)
  where
    iter [] = []
    iter (x:xs)
      | 4 <= length (x:xperm) = (x:xperm) : iter (removeOnes xperm xs)
      | otherwise = iter (removeOnes xperm xs)
      where
        xperm = getPermutesOf x xs
