module LinkedList(new, nil, isNil, datum, next, fromList, toList, reverseLinkedList) where

data LinkedList a = Nil | Cons { datum :: a, next :: LinkedList a }

new = Cons

nil = Nil

isNil Nil = True
isNil _ = False

fromList = foldr Cons Nil

toList Nil = []
toList (Cons d n) = d : toList n

reverseLinkedList = fromList . reverse . toList
