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
-- from 5_4

size :: Set a -> Int
size Empty = 0
size (Node _ left right) = size left + 1 + size right

------------------------------------------------

type Dict = Set String -- list -> set

readDict :: IO Dict
readDict = do
            txt <- readFile "/usr/share/dict/words"
            return (fromList (words txt)) -- added fromList

reverseVideo :: String
reverseVideo = "\ESC[7m"

normalVideo :: String
normalVideo = "\ESC[0m"

main1 :: IO()
main1 = do 
            dict <- readDict
            print (size dict) -- length -> size

checkWord :: Dict -> String -> String
checkWord dict w | member w dict = w -- elem -> member
                 | otherwise = reverseVideo ++ w ++ normalVideo
             
spellCheck dict inputTxt =
    let
        textLines :: [String]
        textLines = lines inputTxt

        allWordsByLine :: [[String]]
        allWordsByLine = map words textLines

        checkedWordsByLine :: [[String]]
        checkedWordsByLine = map (map (checkWord dict)) allWordsByLine

        checkedLines :: [String]
        checkedLines = map unwords checkedWordsByLine
    
        finalTxt :: String
        finalTxt = unlines checkedLines
    in 
        finalTxt

main :: IO()
main = do
            dict <- readDict
            inputTxt <- getContents
            putStr(spellCheck dict inputTxt)