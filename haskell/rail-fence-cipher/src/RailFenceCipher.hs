module RailFenceCipher (encode, decode) where

import qualified Data.Array as Array
import Data.Array ((//), (!))
import Data.List (transpose)

rail :: Int -> Int -> Int
rail n pos = n - 1 - abs ((pos `mod` (n * 2 - 2)) - (n - 1))

encode :: Int -> String -> String
encode n plaintext = concat . go plaintext 0 . Array.listArray (0, n - 1) . replicate n $ id
  where
    go :: String -> Int -> Array.Array Int (String -> String) -> [String]
    go [] _ acc = map ($ "") . Array.elems $ acc
    go (x:xs) pos acc = let i = rail n pos
                            newAcc = acc // [(i, (acc ! i) . (:) x)]
                        in go xs (pos + 1) newAcc

decode :: Int -> String -> String
decode n ciphertext = zigzag 0 . Array.listArray (0, n - 1) . rails 0 $ ciphertext
  where
    l = length ciphertext
    p = n * 2 - 2

    rails :: Int -> String -> [String]
    rails i xs = chunk : rails (i + 1) rest
      where
        k | i == 0 = l `div` p + fromEnum (l `mod` p > 0)
          | i == n - 1 = l `div` p + fromEnum (l `mod` p > i)
          | otherwise = 2 * (l `div` p)
                      + fromEnum (l `mod` p > i)
                      + fromEnum (l `mod` p > (p - i))
        (chunk, rest) = splitAt k xs

    zigzag :: Int -> Array.Array Int String -> String
    zigzag pos xs = if null r then [] else head r : zigzag (pos + 1) (xs // [(i, tail r)])
        where i = rail n pos
              r = xs ! i
