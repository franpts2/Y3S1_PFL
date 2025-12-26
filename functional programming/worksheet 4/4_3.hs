hamming :: [Integer]
hamming = [y | x<-[0..], y <- nums x ]
    where nums n = [2^i*3^j*5^k | i<-[0..n], j<-[0..n], k<-[0..n], i+j+k==n]