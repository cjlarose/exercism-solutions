module Sublist where

data Sublist = Equal | Sublist | Superlist | Unequal deriving (Eq, Show)

isPrefix [] ys = True
isPrefix xs [] = False
isPrefix (x:xs) (y:ys) = x == y && isPrefix xs ys

isSublist [] ys = True
isSublist xs [] = False
isSublist xs ys = isPrefix xs ys || isSublist xs (tail ys) 

sublist xs ys = if isSublist xs ys
                then (if isSublist ys xs then Equal else Sublist)
                else (if isSublist ys xs then Superlist else Unequal)
