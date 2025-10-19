nub :: Eq a => [a] -> [a]
nub [] = []
nub (x:xs) = x : nub (remove x xs)
    where remove _ [] = []
          remove y (z:zs) | y==z = remove y zs
                          | otherwise = z : remove y zs

-- n consegui