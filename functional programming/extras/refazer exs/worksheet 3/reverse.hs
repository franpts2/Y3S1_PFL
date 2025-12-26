myreverse :: Ord a => [a] -> [a]
myreverse = foldr (\n acc -> acc ++ [n]) [] 

myreverse' :: Ord a => [a] -> [a]
myreverse' = foldl (\acc n -> [n] ++ acc) [] 
