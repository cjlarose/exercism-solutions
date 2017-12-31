module Pangram (isPangram) where

import Data.Char (toLower, isAlpha)
import Data.List (sort, nub)

isPangram :: String -> Bool
isPangram = (== ['a'..'z'])
          . sort
          . nub
          . map toLower
          . filter isAlpha
