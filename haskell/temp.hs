module Temp where

import Data.List

take' n (x:xs)
  | n == 0 = []
  | x > 0 = [x] ++ take' (n-1) xs


sum [] = 0
sum (x:xs) = x + (sum xs)
