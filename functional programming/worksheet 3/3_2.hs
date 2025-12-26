-- a) (n consegui)
leastDiv :: Integer -> Integer
leastDiv 0 = 0
leastDiv 1 = 1
leastDiv n = loop 2
    where loop i
            | i > (floor . sqrt . fromIntegral) n = n  -- i > sqrt n   -> n
            | n `mod` i == 0 = i                       -- n mod i == 0 -> i
            | otherwise = loop (i + 1)

-- b)
isPrimeFast :: Integer -> Bool
isPrimeFast n | n>1 && leastDiv n == n = True
              | otherwise = False
