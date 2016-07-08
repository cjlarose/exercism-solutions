module ETL (transform) where

import qualified Data.Map as Map
import Data.Char (toLower)

transform :: Map.Map Int [String] -> Map.Map String Int
transform = Map.fromList . concat . Map.elems . Map.mapWithKey transformLetters where
  transformLetters p = map (\x -> (map toLower x, p))
