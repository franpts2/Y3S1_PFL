toBits :: Int -> [Int]
toBits x = reverse (aux x) 

aux :: Int -> [Int]
aux 1 = [1]
aux n = n`mod`2 : aux (n`div`2)