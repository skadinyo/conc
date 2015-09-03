module Alpha where

import Math
import Clojure
import Data.List
import Data.Char 

eul76 target coins
  | target < 0 = 0
  | target <= 1 = 1
  | null coins = 0
  | otherwise = (eul76 (target - (head coins)) coins) + (eul76 target (tail coins))

powerMe a n
  | n == 0 = 1
  | n == 1 = a
  | otherwise = a * powerMe a (pred n)
