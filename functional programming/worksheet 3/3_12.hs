fromBits :: [Int] -> Int
fromBits ls = foldl (\acc x -> acc*2 + x) 0 ls