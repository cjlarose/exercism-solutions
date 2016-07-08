module LeapYear where

isLeapYear x = x `mod` 4 == 0 && x `mod` 100 /= 0 || x `mod` 400 == 0
