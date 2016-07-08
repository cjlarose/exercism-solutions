module SumOfMultiples where

import Data.List (nub)

multiplesOf k = map (* k) [1 ..]

sumOfMultiples ks limit = sum . nub $ concatMap f ks where
  f k = takeWhile ((>) limit) $ multiplesOf k
