module RunLength (decode, encode) where

import Data.Char (isDigit)
import Data.List (findIndex, splitAt, group, span)
import Data.Maybe (fromJust)
import Control.Arrow (first)

decode :: String -> String
decode "" = ""
decode encodedText = replicate len chunk ++ decode rest
  where
    (len, chunk:rest) = if isDigit (head encodedText)
                        then first read $ span isDigit encodedText
                        else (1, encodedText)

encode :: String -> String
encode = concatMap f . group
  where
    f [c] = [c]
    f chunk = show (length chunk) ++ [head chunk]
