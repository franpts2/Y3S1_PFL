leftHalf :: [Int] -> [Int]
leftHalf l = take (div (length l) 2) l

rightHalf :: [Int] -> [Int]
rightHalf l = drop (div (length l) 2) l

