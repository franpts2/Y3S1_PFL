mediana :: Int -> Int -> Int -> Int
mediana a b c 
    | (b == max3 && a >= c) || (c == max3 && a >= b) = a
    | (a == max3 && b >= c) || (c == max3 && b >= a) = b
    | otherwise = c
    where max3 = max (max a b) c