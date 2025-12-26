group :: Eq a => [a] -> [[a]]
group [] = []
group (x:xs) =  first_group : group remaining
    where 
        first_group = x : takeWhile (==x) xs
        remaining = dropWhile (==x) (x:xs)