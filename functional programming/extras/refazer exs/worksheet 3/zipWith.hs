zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] [] = []
zipWith' f (x:xs) (y:ys) = (f x y) : zipWith' f xs ys  
