-- a)
max3 :: Ord a => a -> a -> a -> a
max3 a b c | (a>=b && a>=c) = a 
           | (b>=a && b>=c) = b
           | (c>=a && c>=b) = c

min3 :: Ord a => a -> a -> a -> a
min3 a b c | (a<=b && a<=c) = a 
           | (b<=a && b<=c) = b
           | (c<=a && c<=b) = c

-- b)
max3' :: Ord a => a -> a -> a -> a
max3' a b c = max max_bc a
    where max_bc = max b c

min3' :: Ord a => a -> a -> a -> a
min3' a b c = min min_bc a
    where min_bc = min b c