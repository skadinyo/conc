module Solution4 where

import Problem
import Math
import Clojure
import Data.List
import Data.Char

eul31 target coins
  | target < 0 = 0
  | target <= 1 = 1
  | null coins = 0
  | otherwise = (eul31 (target - (head coins)) coins) + (eul31 target (tail coins))

eul34 lim = sum [ x | x <- [3..lim], x == sum (map factorial (numberDigitsC x))]

----------

base10to2 x
  | x < 2 = [x]
  | otherwise = (base10to2 (fst dm)) ++ [(snd dm)]
  where
    dm = divMod x 2

isPalinBase2 x = xs == reverse xs
  where xs = base10to2 x

eul36 lim = sum $ filter isPalinBase2  $ filter isPalin [1,3..lim]

----------
