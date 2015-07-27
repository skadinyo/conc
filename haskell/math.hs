module Math where
import Data.List
import Data.Bits
import Clojure

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

genPrime = iter 2
  where
    iter x = x : (iter (nextPrime x))

genPrimeBetween a b = iter (if isPrime a then a else nextPrime a)
  where
    iter x
      | x > b = []
      | otherwise = x : iter (nextPrime x)

divUntil n x
  | 0 == rem n x = divUntil (div n x) x
  | otherwise = n

numberDigits x
  | x < 10 = [x]
  | otherwise = (rem x 10) : numberDigits (div x 10)

numberDigitsC x
  | x < 10 = [x]
  | otherwise = concat [numberDigits (div x 10), [(rem x 10)]]

sumDigits x
  | x < 10 = x
  | otherwise = (rem x 10) + sumDigits (div x 10)

isPalin x = p == reverse p
  where
    p = numberDigits x

divisors x = iter 1 []
  where
    limit = floor $ sqrt (fromIntegral x)
    iter i res
      | i > limit = []
      | 0 == (rem x i) = if (and [(i == limit),(i*i == x)]) then i : [] else i : (div x i) : (iter (succ i) res)
      | otherwise = iter (succ i) res

properDivisors x = iter 2
  where
    limit = floor $ sqrt (fromIntegral x)
    iter i
      | i > limit = [1]
      | 0 == (rem x i) = if (and [(i == limit),(i*i == x)]) then i : [1] else i : (div x i) : (iter (succ i))
      | otherwise = iter (succ i)

primeFactors x = iter 2 x
  where
    iter _ 1 = []
    iter p temp
      | 0 == rem temp p = p : iter (nextPrime p) d
      | otherwise = iter (nextPrime p) temp
      where
        d = divUntil temp p

totient n = iter n (primeFactors n)
  where
    iter x [] = x
    iter x (i:is) = iter (div (x * (pred i)) i) is

countPrimeFactors x = iter 2 x
  where
    iter _ 1 = 0
    iter p temp
      | 0 == rem temp p = 1 + iter (nextPrime p) d
      | otherwise = iter (nextPrime p) temp
      where
        d = divUntil temp p

chainPrimeFactor x = iter x 2 []
  where
    iter 1 _ res = res
    iter i p res
      | 0 == rem i p = iter (div i p) p (p:res)
      | otherwise = iter i (nextPrime p) res

countFactor 0 = 0
countFactor 1 = 1
countFactor x = reduce (*) (map succ (map length (group (chainPrimeFactor x))))

nthPrime nt = iter 1 2
  where
    iter n p
      | n == nt = p
      | otherwise = iter (succ n) (nextPrime p)

pascalTriangle = iter []
  where
    iter [] = [1] : iter [1]
    iter x = n : iter n
      where
        n = map2 (+) (x++[0]) ([0]++x)

fibo = iter 0 1
  where
    iter a b = a : (iter b (a + b))

pow2 n = unsafeShiftL 1 n

factorial 0 = 1
factorial 1 = 1
factorial n = n * factorial (pred n)

countNumber n
  | n < 10 = 1
  | otherwise = 1 + countNumber (div n 10)

triangleNumber = iter 0 1
  where
    iter a n = a : (iter (a + n) (succ n))

isPermutes a b = sort (numberDigits a) == sort (numberDigits b)

getPermutesOf i xs = iter xs
  where
    iref = sort (numberDigits i)
    iter [] = []
    iter (y:ys)
      | yref == iref = y : (iter ys)
      | otherwise = iter ys
      where
        yref = sort (numberDigits y)

sumDigitsFactorial x = sum (map factorial (numberDigits x))

isAbundant x = x < sum (properDivisors x)
