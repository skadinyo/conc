module Alpha where

import Math
import Clojure

eul1 lim = sum [x | x <- [1..(pred lim)] , or [(0 == rem x 3),(0 == rem x 5)]]

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
