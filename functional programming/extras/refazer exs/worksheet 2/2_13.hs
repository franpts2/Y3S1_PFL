binom :: Integer -> Integer -> Integer
binom n k = (product [1..n]) `div` ((product [1..k]) * (product [1..n-k]))

-- extra
binom' :: Integer -> Integer -> Integer
binom' n k = (product [n-k+1..n]) `div` (product [1..k])