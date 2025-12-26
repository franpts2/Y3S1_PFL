primo :: Integer -> Bool
primo n | n > 1 && all (\d -> n `mod` d == 0) [2..sqrti] = True
	| otherwise = False
	where sqrti = floor (sqrt (fromIntegral n))
