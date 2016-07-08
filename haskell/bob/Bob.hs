module Bob where

import Data.Char (isAlpha, isUpper, isSpace)

isQuestion xs = last xs == '?'
isYelling xs = any isAlpha xs && all isUpper letters
isBlank = all isSpace

responseFor x
  | isBlank x    = "Fine. Be that way!"
  | isYelling x  = "Whoa, chill out!"
  | isQuestion x = "Sure."
  | otherwise    = "Whatever."
