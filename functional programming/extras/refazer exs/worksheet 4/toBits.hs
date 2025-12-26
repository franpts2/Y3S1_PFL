toBits :: Int -> [Int]
toBits n = reverse (toBitsRev n)

toBitsRev :: Int -> [Int]
toBitsRev n | n<2 = [n]
toBitsRev n = bit : toBitsRev resto
	where bit = n `mod` 2
	      resto = n `div` 2
