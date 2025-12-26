-- a) 
append' :: [a] -> [a] -> [a]
append' xs ys = foldr (:) ys xs

-- b)
concat' :: [[a]] -> [a]
concat' ls = foldr (++) [] ls

-- c)
reverse1 :: [a] -> [a]
reverse1 ls = foldr (\x acc -> acc ++ [x]) [] ls

-- d)
reverse2 :: [a] -> [a]
reverse2 ls = foldl (\acc x -> [x] ++ acc) [] ls

-- e)
elem' :: Eq a => a -> [a] -> Bool
elem' x ls =  any (\n -> n==x) ls