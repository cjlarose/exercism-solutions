module RunLength (decode, encode) where

import Data.Char (isDigit)
import Data.List (group, span)

decode :: String -> String
decode "" = ""
decode encodedText@(h:t)
  | isDigit h = chunk . span isDigit $ encodedText
  | otherwise = h : decode t
  where chunk (l,c:rest) = replicate (read l) c ++ decode rest

encode :: String -> String
encode = concatMap f . group
  where
    f [c] = [c]
    f chunk = show (length chunk) ++ [head chunk]
