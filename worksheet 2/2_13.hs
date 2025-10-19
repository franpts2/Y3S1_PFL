-- a)
binom :: Integer -> Integer -> Integer
binom n k = factorial n `div` (factorial k * factorial (n - k))
    where factorial x = product [1..x]

-- b)
pascal :: Integer -> [[Integer]]
pascal n = [line y | y<-[0..n]]
    where line x = [binom x a | a<-[0..x]]