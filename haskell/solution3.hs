module Solution3 where

import Problem
import Math
import Clojure
import Data.List
import Data.Char

eul21 limit = sum $ iter [2..limit] []
  where
    iter [] res = res
    iter (x:xs) res
      | x == sumdivx = (iter xs res)
      | x == sumdivy = x : (iter xs res)
      | otherwise = iter xs res
        where
          sumdivx = sum (properDivisors x)
          sumdivy = sum (properDivisors sumdivx)

eul24 _ [] = []
eul24 n elements = iter 0 divn
  where
    c = length elements
    maxn = factorial c
    divn = div maxn c
    iter i t
      | n < t = (elements !! i) : (eul24 (n - (t - divn)) (removeElement i elements))
      | otherwise = iter (succ i) (t + divn)

eul25 n = iter 1 1 1
  where
    lim = 10^(pred n)
    iter a b c
      | a > lim = c
      | otherwise = iter b (a + b) (succ c)

----------

countReciprocal x = iter 1 []
  where
    iter i temp
      | elem (snd dm) temp = length temp
      | otherwise = iter (10 * (snd dm)) ((snd dm) : temp)
        where
          dm = divMod i x

----------

eul26 lim = maxBy countReciprocal [1..lim]

eul29 lim = length $ nub [ a^b | a <- [2..lim], b <- [2..lim]]

eul30a n = sum [ x | x <- [2..(9^n * (n - 1))], x == sum (map (\c -> c^n) (numberDigits x))]

eul30b n = sum [ x | x <- [2..(9^n * (n - 1))], x == sum (map (\c -> (ref !! c)) (numberDigits x))]
  where
    ref = map (\c -> c^n) [0..9]
