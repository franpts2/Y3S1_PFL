-- a)
insert :: Ord a => a -> [a] -> [a]
insert a [] = [a]
insert n (x:xs) | n<=x = n : x : xs
                | otherwise = x : insert n xs

-- b)
isort :: Ord a => [a] -> [a]
isort [] = []
isort (x:xs) = insert x (isort xs)