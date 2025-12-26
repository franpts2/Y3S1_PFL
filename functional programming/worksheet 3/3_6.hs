-- a)
merge :: Ord a => [a] -> [a] -> [a]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x <= y = x : merge xs (y:ys)
                    | otherwise = y : merge (x:xs) ys

-- b)
msort :: Ord a => [a] -> [a]
msort []  = []
msort [a] = [a]
msort ls  = merge (msort left) (msort right)
    where
        mid   = length ls `div` 2
        left  = take mid ls
        right = drop mid ls
