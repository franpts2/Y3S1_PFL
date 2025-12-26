intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse n (x:xs) = [x] ++ [n] ++ intersperse n xs
