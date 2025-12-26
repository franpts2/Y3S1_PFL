-- a)
and' :: [Bool] -> Bool
and' [] = True
and' (x:xs) = (x || False) && and' xs

-- b)
or' :: [Bool] -> Bool
or' [] = False
or' (x:xs) = (x && True) || or' xs

-- c)
concat' :: [[a]] -> [a]
concat' [] = []
concat' (x:xs) = x ++ concat' xs

-- d)
replicate' :: Int -> a -> [a]
replicate' 0 _ = []
replicate' n x = [x] ++ replicate' (n-1) x

-- e) (unica q n consegui)
index' :: [a] -> Int -> a -- index the n-th value in a list (starting from zero);
index' (x:_) 0 = x
index' (_:xs) n = index' xs (n-1)
index' [] _ = error "index out of bounds"

-- f)
elem' :: Eq a => a -> [a] -> Bool
elem' _ [] = False
elem' a (x:xs) | x==a = True
               | otherwise = elem' a xs