module Clojure where

reduce f (x:[]) = x
reduce f (x:y:xs) = reduce f ((f x y) : xs)

reduceI f ini xs = reduce f (init:xs)

reductions f (x:[]) = [x]
reductions f (x:y:xs) = n : reductions f (n:xs)
  where
    n = f x y

reductionsI f ini xs = reductions f (ini:xs)
