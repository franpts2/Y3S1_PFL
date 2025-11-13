-- a)
merge :: [Integer] -> [Integer] -> [Integer]
merge [] ys = ys
merge xs [] = xs
merge (x:xs) (y:ys) | x < y = x : merge xs (y:ys)
                    | x > y = y : merge (x:xs) ys 
                    | otherwise = x : merge xs ys

-- b)
hamming :: [Integer]
hamming = 1 : merge (map (*5) hamming) (merge (map (*2) hamming) (map (*3) hamming))