fromBits :: [Int] -> Int
fromBits lst = fromBitsAcc (reverse lst) 0

fromBitsAcc :: [Int] -> Int -> Int
fromBitsAcc [] _ = 0
fromBitsAcc (x:xs) acc = x*(2^acc) + fromBitsAcc xs (acc+1)
