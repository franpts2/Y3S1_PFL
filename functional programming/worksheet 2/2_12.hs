concat' :: [[a]] -> [a]
concat' ls = [x | xs <- ls, x <- xs]

replicate' :: Int -> a -> [a]
replicate' n x = [ x | _ <- [1..n] ]

idx' :: [a] -> Int -> a
idx' ls n = head [ x | (x,i) <- zip ls [0..], i == n ]
