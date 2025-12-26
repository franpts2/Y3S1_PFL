mysort :: Ord a => [a] -> [a]
mysort [] = []
mysort [a] = [a]
mysort (x:xs) = insert x (mysort xs)

insert :: Ord a => a -> [a] -> [a]
insert n [] = [n]
insert n (x:xs) | n <= x = [n] ++ [x] ++ xs
		| otherwise = [x] ++ insert n xs
