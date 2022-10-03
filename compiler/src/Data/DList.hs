module Data.DList where

type DList a = [a] -> [a]

empty :: DList a
empty = \xs -> xs

append :: a -> DList a
append a = \as -> a : as

concat :: DList a -> DList a -> DList a
ys `concat` zs = \as -> ys (zs as)

fromList :: [a] -> DList a
fromList ys = \xs -> ys ++ xs

toList :: DList a -> [a]
toList as = as []
