module Anagram (anagramsFor) where

import Data.List (sort)
import Data.Char (toLower)

anagrams xs ys = xs /= ys && sort xs == sort ys
anagramsFor x = filter (\y -> anagrams (map toLower x) (map toLower y))
