module School (School, empty, add, grade, sorted) where

import qualified Data.Map as Map
import qualified Data.Set as Set

type School = Map.Map Int (Set.Set String)

empty :: School
empty = Map.empty

add :: Int -> String -> School -> School
add g = Map.insertWith Set.union g . Set.singleton

grade :: Int -> School -> [String]
grade = (Set.toAscList .) . Map.findWithDefault Set.empty

sorted :: School -> [(Int, [String])]
sorted = Map.toList . Map.map Set.toAscList
