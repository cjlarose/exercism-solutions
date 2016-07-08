module Hamming where

distance :: Eq a => [a] -> [a] -> Int
distance = ((sum . map fromEnum) .) . zipWith (/=)
