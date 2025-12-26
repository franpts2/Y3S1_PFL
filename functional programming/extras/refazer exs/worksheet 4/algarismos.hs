algarismos :: Int -> [Int]
algarismos n = reverse (algarismosRev n)

algarismosRev :: Int -> [Int]
algarismosRev n | n<10 = [n]
algarismosRev n = [digit] ++ algarismosRev resto
	where digit = n `mod` 10
	      resto = n `div` 10
