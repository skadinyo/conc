module Temp where

import Data.List

fy x = ix^2 + ix
  where ix = floor x

pascalTri x = x:pascalTri ([1] ++ m ++ [1])
  where
    m = zipWith (+) (init x) (tail x)

-- 1.a

null' [] = True
null' _ = False

--pembatas

take' n (x:xs)
  | n == 0 = []
  | n > 0 = [x] ++ take' (n-1) xs
take' _ [] = []


takeDropWhile f coll = iter coll []
  where
    iter [] temp = ((reverse temp),[])
    iter (x:xs) temp
      | f x = iter xs (x:temp)
      | otherwise = ((reverse temp),x:xs)

--

group' [] = []
group' (x:xs) = (x:h) : (group' s)
  where
    (h,s) = takeDropWhile (x==) xs


---

unicorn x y = nub $ x ++ y
union'' [] [] = []
union'' (x:xs) [] = (x:xs)
union'' [] (y:ys) = (y:ys)
union'' (x:xs) (y:ys)
  | y == temp y (x:xs) = union'' (x:xs) ys
  | y /= temp y ((x:xs)) = union'' ((x:xs ++ [y])) ys
    where temp y [] = 0
          temp y (x:xs)
           | y == x = y
           | y /= x = temp y xs
