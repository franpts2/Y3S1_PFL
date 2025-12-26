merge :: Ord a => [a] -> [a] -> [a]
merge xs [] = xs
merge [] ys = ys
merge (x:xs) ys = insert x (merge xs ys)

insert :: Ord a => a -> [a] -> [a]
insert n [] = [n]
insert n (x:xs) | n <= x = [n] ++ [x] ++ xs
		| otherwise = x : insert n xs


