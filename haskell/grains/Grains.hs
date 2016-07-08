module Grains where

import Data.Bits (shiftL)

square :: Int -> Integer
square x = 1 `shiftL` (x - 1)

total = (square 65) - 1
