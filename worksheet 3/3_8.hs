fromBits :: [Int] -> Int
fromBits bits = aux bits 0

aux :: [Int] -> Int -> Int
aux [] a     = a
aux (x:xs) a = aux xs (a * 2 + x)