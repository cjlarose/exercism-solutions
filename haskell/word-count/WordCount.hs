module WordCount(wordCount) where

import qualified Data.Map as Map
import Text.Regex (mkRegex, splitRegex)
import Data.Maybe (fromMaybe)
import Data.Char (toLower)

extractWords = map (map toLower) . filter (not . null) . splitRegex (mkRegex "[^[:alnum:]]+")

wordCount :: String -> Map.Map String Int
wordCount = Map.fromListWith (+) . map (\x -> (x, 1)) . extractWords
