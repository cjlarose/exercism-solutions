module RailFenceCipher (encode, decode) where

import Data.List (init, sortOn, sort)

encode :: Int -> [a] -> [a]
encode n = map snd . sortOn fst . zip p
  where
    ii = [0..(n-1)]
    p = cycle $ ii ++ reverse (drop 1 . init $ ii)

decode :: Int -> String -> String
decode n ciphertext = map snd . sort . zip (encode n [0..(l-1)]) $ ciphertext
  where
    l = length ciphertext
