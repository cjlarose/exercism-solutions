module Strain where

keep f [] = []
keep f (x:xs) = if f x then x : keep f xs else keep f xs

discard = keep . (not .)
