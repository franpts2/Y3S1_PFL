-- a)
median :: Ord a => a -> a -> a -> a
median x y z | (x<=y && x>=z) || (x<=z && x>=y) = x 
             | (y<=x && y>=z) || (y<=z && y>=x) = y
             | (z<=y && z>=x) || (z<=x && z>=y) = x 

-- b)
median' :: (Num a, Ord a)  => a -> a -> a -> a
median' x y z = (x+y+z) - maxi - mini    
        where 
            maxi = max x (max y z)
            mini = min x (min y z)