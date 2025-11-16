{-
 A module for sets implemented using binary search trees
 Pedro Vasconcelos, 2025
-}
module Set (Set,
            empty, fromList,
            insert, member) where

import Data.List (sort)

data Set a
  = Empty
  | Node a (Set a) (Set a)
  -- opaque type:
  -- don't export the constructors
  -- don't derive show

empty :: Set a
empty = Empty

insert :: Ord a => a -> Set a -> Set a
insert x Empty = Node x Empty Empty
insert x (Node y left right)
  | x < y = Node y (insert x left) right
  | x > y = Node y left (insert x right)
  | otherwise = Node y left right

member :: Ord a => a -> Set a -> Bool
member x Empty = False
member x (Node y left right)
  | x == y = True
  | x < y = member x left
  | otherwise = member x right

fromList :: Ord a => [a] -> Set a
fromList xs = build (sort xs)
   where
     build [] = Empty
     build xs = Node x (build xs') (build xs'')
       where
         k = length xs `div` 2
         xs' = take k xs
         x:xs''= drop k xs

------------------------------------------------

-- a)
size :: Set a -> Int
size Empty = 0
size (Node _ left right) = size left + 1 + size right

-- b)
height :: Set a -> Int
height Empty = 0
height (Node _ left right) 
    | heightLeft > heightRight = heightLeft
    | otherwise = heightRight
    where 
        heightLeft = height left + 1
        heightRight = height right + 1

-- c)
-- set2 search is more efficient than set1. 
-- set1 will become a very unbalanced tree with height 1000, because the list is sorted
-- set2 makes it so the tree is way more balanced
-- therefore, searching in set2 will be way more efficient