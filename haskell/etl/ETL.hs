module ETL (transform) where

import qualified Data.Map as Map
import Data.Char (toLower)

transform :: Map.Map Int [String] -> Map.Map String Int
transform = Map.fromList . concatMap transformLetters . Map.toList where
  transformLetters (p, letters) = zip (map (map toLower) letters) (repeat p)
