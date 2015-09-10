module Temp where

import Data.List

foldl1'' f i [] = i
foldl1'' f i (x:xs) = foldl1'' f (f i x) xs

union'' x y = nub (x ++ y)

scanl'' f i [] = []
scanl'' f i (x:xs) = temp : (scanl'' f temp xs)
  where temp = (f i x)

scanl1'' f [] = []
scanl1'' f [x] = [x]
scanl1'' f (x:xs) = temp : (scanl1'' f (temp:(tail xs)))
  where
     temp = (f x (head xs))

sort' [] = []
sort' (x:xs) = [zazan (x:xs)] ++ sort' (delete' (zazan (x:xs)) (x:xs))
  where zazan [z] = z
        zazan (x:xs) = min (x) (zazan xs)

-- 1.a

null' [] = True
null' _ = False

--pembatas

take' _ [] = []
take' 0 _ = []
take' n (x:xs) = x : (take' (pred n) xs)

--pembatas

drop' _ [] = []
drop' 0 xs = xs
drop' n (x:xs) = drop' (pred n) xs

--pembatas

fst' (a,b) = a

--pembatas

snd' (a,b) = b

--pembatas

map' _ [] = []
map' f (x:xs) = (f x) : (map' f xs)

--pembatas

filter' f [] = []
filter' f (x:xs)
  | (f x) = x : (filter' f xs)
  | otherwise = (filter' f xs)

--pembatas

delete' _ [] = []
delete' x (i:is)
  | x == i = is
  | otherwise = i : (delete' x is)

--pembatas

deleteAll' _ [] = []
deleteAll' x (i:is)
  | x == i = (deleteAll' x is)
  | otherwise = i : (deleteAll' x is)

--pembatas

--foldl' f x [] = x
--foldl' f x (y:ys) = foldl' f (f x y) ys

--pembatas

--foldl1' x = x

--pembatas

zip' [] _ = []
zip' _ [] = []
zip' (x:xs) (y:ys) = (x,y) : (zip' xs ys)

--pembatas

zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = (f x y) : (zipWith' f xs ys)

--pembatas

nth' (x:xs) 0 = x
nth' (x:xs) n = nth' xs (n-1)

--pembatas

scanl' x = x

--pembatas

scanl1' x = x

--pembatas

elem' x = x

--pembatas

notElem' x = x

--pembatas

head' x = x

--pembatas

length' x = x

--pembatas

reverse' x = x

--pembatas

last' x = x

--pembatas

tail' x = x

--pembatas

init' x = x

--pembatas

max' x = x

--pembatas

min' x = x

--pembatas

concat' x = x

--pembatas

intersperse' x = x

--pembatas

intercalate' x = x

--pembatas

and' x = x

--pembatas

or' x = x

--pembatas

zip3' x = x

--pembatas

sum' x = x

--pembatas

product' x = x

--pembatas

words' x = x

--pembatas

lines' x = x

--pembatas

unlines' x = x

--pembatas

unwords' x = x

--pembatas

takeWhile' x = x

--pembatas

dropWhile' x = x

--pembatas

concatMap' x = x

--pembatas

all' x = x

--pembatas

any' x = x

--pembatas

insert' x = x

--pembatas

zipWith3' x = x

--pembatas

-- 1.b

nub' x = x

--pembatas

sort' x = x

--pembatas

minimum' x = x

--pembatas

maximum' x = x

--pembatas

inits' x = x

--pembatas

tails' x = x

--pembatas

union' x = x

--pembatas

intersect' x = x

--pembatas

group' x = x

--pembatas

splitAt' x = x

--pembatas

partition' x = x

--pembatas

replicate' x = x

--pembatas
-- First Assignment
-- Reimplement Haskell function
-- DON'T USE GOOGLE
