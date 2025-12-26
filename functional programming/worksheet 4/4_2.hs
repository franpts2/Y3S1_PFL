twinPrimes :: [(Integer, Integer)]
twinPrimes = [(a,b) | (a,b) <- zip primes (tail primes), b-a==2]

primes = sieve [2..]

sieve :: [Integer] -> [Integer]
sieve (p:xs) = p : sieve [x | x <- xs, x`mod`p/=0]