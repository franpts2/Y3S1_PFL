-- a)
short :: [a] -> Bool
short l = length l <= 3

-- b)
short' :: [a] -> Bool
short' l | length l == 1 = True 
short' l | length l == 2 = True 
short' l | length l == 3 = True
short' l = False 