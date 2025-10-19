perfects :: Integer -> [Integer]
perfects n = [x | x <- [1..n], x==sum (propDivs x)]

propDivs :: Integer -> [Integer]
propDivs n = [ d | d <- [1..n-1], n `mod` d == 0]