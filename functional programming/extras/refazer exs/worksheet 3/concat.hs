myconcat :: Ord a => [a] -> [a] -> [a]
myconcat xs ys = foldr (:) ys xs
