divprop :: Integer -> [Integer]
divprop n = [d | d<-[1..n-1], n `mod` d == 0]