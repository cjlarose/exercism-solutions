module RailFenceCipher (encode, decode) where

import Data.List (sortOn, sort)

encode :: Int -> [a] -> [a]
encode n = map snd . sortOn fst . zip p
  where p = cycle $ [0..(n-1)] ++ [n-2,n-3..1]

decode :: Int -> String -> String
decode n xs = map snd . sort . zip ii $ xs
  where ii = encode n [0..(length xs-1)]
