-- a)
intercalate :: a -> [a] -> [[a]]
intercalate x [] = [[x]]
intercalate x (y:ys) = first : rest
    where 
        first = x : (y:ys) 
        rest = map (y : ) (intercalate x ys)

-- b)
permutations :: [a] -> [[a]]
permutations [] = [[]]
permutations (x:xs) = concat perms
    where 
        tail_perms = permutations xs
        perms = map (intercalate x) tail_perms
