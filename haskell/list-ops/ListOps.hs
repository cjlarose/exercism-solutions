module ListOps
  ( length
  , reverse
  , map
  , filter
  , foldr
  , foldl'
  , (++)
  , concat
  ) where

import Prelude hiding
  ( length, reverse, map, filter, foldr, (++), concat )

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ i [] = i
foldl' f i (x:xs) = let z' = f i x in z' `seq` foldl' f z' xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr _ i [] = i
foldr f i (x:xs) = f x (foldr f i xs)

length :: [a] -> Int
length = foldl' (\a _ -> succ a) 0

reverse :: [a] -> [a]
reverse = foldl' (flip (:)) []

map :: (a -> b) -> [a] -> [b]
map f = foldr (\x a -> f x : a) []

filter :: (a -> Bool) -> [a] -> [a]
filter f = foldr (\x a -> if f x then x : a else a) []

(++) :: [a] -> [a] -> [a]
xs ++ ys = foldr (:) ys xs

concat :: [[a]] -> [a]
concat = foldr (++) []
