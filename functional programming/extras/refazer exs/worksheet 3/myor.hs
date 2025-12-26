myor :: [Bool] -> Bool
myor [] = False
myor (x:xs) = x || myor xs
