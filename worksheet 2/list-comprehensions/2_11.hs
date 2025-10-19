isPrime :: Integer -> Bool
isPrime x | divs x == [1,x] = True
          | otherwise = False

divs :: Integer -> [Integer]
divs n = [d | d<-[1..n], n `mod` d == 0]