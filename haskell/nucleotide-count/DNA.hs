module DNA (count, nucleotideCounts) where

import qualified Data.Map as Map
import Data.Either (partitionEithers)

data Nucleotide = A | T | C | G deriving (Show, Eq)

readNucleotide :: Char -> Either String Nucleotide
readNucleotide 'A' = Right A
readNucleotide 'T' = Right T
readNucleotide 'C' = Right C
readNucleotide 'G' = Right G
readNucleotide x = Left ("invalid nucleotide '" ++ [x] ++ "'")

readStrand :: String -> Either String [Nucleotide]
readStrand xs = if null errors then Right results else Left (head errors) where
  (errors, results) = partitionEithers $ map readNucleotide xs

countOccurances :: Eq a => a -> [a] -> Int
countOccurances n = length . filter (== n)

count :: Char -> String -> Int
count n xs = either error id total where
  total = countOccurances <$> readNucleotide n <*> readStrand xs

nucleotideCounts :: String -> Map.Map Char Int
nucleotideCounts xs = either error runCounts (readStrand xs) where
  showNucleotide = head . show
  runCounts :: [Nucleotide] -> Map.Map Char Int
  runCounts ns = Map.fromList $ map (\x -> (showNucleotide x, countOccurances x ns)) [A, T, C, G]
