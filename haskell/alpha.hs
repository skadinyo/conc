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
