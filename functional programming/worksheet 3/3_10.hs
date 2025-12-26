isPrimeFast :: Integer -> Bool 
isPrimeFast n = (n>1) && all (\d -> n`mod`d /= 0) [2..sqrti]
    where sqrti = floor (sqrt (fromIntegral n))