intersperse :: a -> [a] -> [a]
intersperse _ [] = []
intersperse _ [x] = [x]
intersperse c (x:xs) = [x] ++ [c] ++ intersperse c xs 