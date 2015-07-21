module Math where

sq x = x * x

isPrime 0 = False
isPrime 1 = False
isPrime 2 = True
isPrime 3 = True
isPrime x
  | 0 == rem x 2 = False
  | 0 == rem x 3 = False
  | otherwise = iter 3
  where
    lim = floor (sqrt $ fromIntegral x)
    iter n
      | n > lim = True
      | 0 == rem x n = False
      | otherwise = iter (n + 2)

nextPrime 1 = 2
nextPrime x = iter (if (even x) then (x + 1) else (x + 2))
  where
    iter i
      | isPrime i = i
      | otherwise = iter (i + 2)

divUntil n x
  | 0 == rem n x = divUntil (div n x) x
  | otherwise = n

numberDigits x
  | x < 10 = [x]
  | otherwise = (rem x 10) : numberDigits (div x 10)

numberDigitsC x
  | x < 10 = [x]
  | otherwise = concat [numberDigits (div x 10), [(rem x 10)]]

isPalin x = p == reverse p
  where
    p = numberDigits x

primeFactors x = iter 2 x
  where
    iter _ 1 = []
    iter p temp
      | 0 == rem temp p = p : iter (nextPrime p) d
      | otherwise = iter (nextPrime p) temp
      where
        d = divUntil temp p
nthPrime nt = iter 1 2
  where
    iter n p
      | n == nt = p
      | otherwise = iter (succ n) (nextPrime p)

dumbPrimeSieve n = iter (2:[3,5..n])
  where
    iter [] = []
    iter (x:xs) = x : iter (filter (\i -> 0 /= rem i x) xs)
