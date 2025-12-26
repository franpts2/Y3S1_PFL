myand :: [Bool] -> Bool
myand [] = True
myand (x:xs) = x && myand xs
