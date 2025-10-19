divisors :: Integer -> [Integer]
divisors n = filter (\d -> mod n d == 0) [1..n]