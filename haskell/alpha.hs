module Alpha where

import Problem
import Math
import Clojure
import Data.List
import Data.Char

eul1 lim = sum [x | x <- [1..(pred lim)] , or [(0 == rem x 3),(0 == rem x 5)]]

eul2 lim = iter 1 1
  where
    iter a b
      | a > lim = 0
      | even a = a + (iter b (b + a))
      | otherwise = iter b (b + a)

eul3 x = iter 2 x
  where
    iter p temp
      | d == 1 = p
      | otherwise = iter (nextPrime p) d
      where
        d = divUntil temp p

eul4 _ = maximum [p | x <- [100..1000], y <- [100..1000], let p = x*y, isPalin p]

eul5 lim = reduce lcm [2..lim]

eul6 lim = squaresum - sumsquare
  where
    squaresum = sq (reduce (+) [1..lim])
    sumsquare = reduce (+) [sq x | x <- [1..lim]]

eul7 x = nthPrime x

eul9 s = [a*b*c | c <- [1..s], b <- [1..c], a <- [1..b], a+b+c == s, a^2 + b^2 == c^2]
-- work but really naive and dumb
eul10 lim = iter 2
  where
    iter p
      | p > lim = 0
      | otherwise = p + iter (nextPrime p)

eul12 fac = iter triangleNumber
  where
    iter (x:xs)
      | (countFactor x) >= fac = x
      | otherwise = iter xs

countCollatz x
  | x == 1 = 1
  | even x = succ (countCollatz (div x 2))
  | otherwise = succ (countCollatz (succ (3 * x)))

eul14 lim = maxBy last (map (\x -> [x,(countCollatz x)]) [1..(pred lim)])

eul15 l = pascalTriangle !! (l+l) !! l

eul16 n = sumDigits $ pow2 n

eul20 n = sumDigits $ factorial n

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

countReciprocal x = iter 1 []
  where
    iter i temp
      | elem (snd dm) temp = length temp
      | otherwise = iter (10 * (snd dm)) ((snd dm) : temp)
        where
          dm = divMod i x

eul26 lim = maxBy countReciprocal [1..lim]

eul29 lim = length $ nub [ a^b | a <- [2..lim], b <- [2..lim]]

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

eul52 = iter 1
  where
    iter x
      | equal $ map sort $ map numberDigits $ map (\c -> c * x) [1,2,3,4,5,6] = x
      | otherwise = iter (succ x)


eul53 lim = length $ filter (\c -> c > 1000000) $ concat $ take (succ lim) pascalTriangle

eul56 lim = maximum [sum $ numberDigits $ a^b | a <- [1..lim], b <- [1..lim]]
