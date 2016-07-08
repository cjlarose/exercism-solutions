module Phone (number, areaCode, prettyPrint) where

import Data.Char (isDigit)
import Data.Maybe (fromJust)

number :: String -> Maybe String
number xs
  | length digits == 10 = Just digits
  | length digits == 11 = if head digits == '1' then Just (tail digits) else Nothing
  | otherwise           = Nothing
  where
    digits = filter isDigit xs

areaCode :: String -> Maybe String
areaCode xs = take 3 <$> number xs

subscriberNumber :: String -> Maybe String
subscriberNumber xs = (f . drop 3) <$> number xs where
  f xs = take 3 xs ++ "-" ++ drop 3 xs

prettyPrint :: String -> Maybe String
prettyPrint xs = format <$> number xs where
  format xs = "(" ++ fromJust (areaCode xs) ++ ") " ++ fromJust (subscriberNumber xs)
