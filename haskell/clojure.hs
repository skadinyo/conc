module Clojure where

reduce f (x:[]) = x
reduce f (x:y:xs) = reduce f ((f x y) : xs)

reduceI f ini xs = reduce f (init:xs)

reductions f (x:[]) = [x]
reductions f (x:y:xs) = n : reductions f (n:xs)
  where
    n = f x y

reductionsI f ini xs = reductions f (ini:xs)

map2 f _ [] = []
map2 f [] _ = []
map2 f (x:xs) (y:ys) = (f x y) : map2 f xs ys

maxBy f [] = error "max of empty ?"
maxBy f (x:[]) = x
maxBy f (x:xs)
  | f x > f n = x
  | otherwise = n
  where
     n = (maxBy f xs)

removeElement n xs = h++t
  where
    s = splitAt n xs
    h = fst s
    t = tail $ snd s

removeOne _ [] = []
removeOne x (i:is)
  | x == i = is
  | otherwise = i : removeOne x is

removeOnes [] is = is
removeOnes (x:xs) is = removeOnes xs (removeOne x is)

equal (x:[]) = True
equal (x:xs)
  | x == (head xs) = equal xs
  | otherwise = False
