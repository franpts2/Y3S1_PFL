calcPi1 :: Int -> Double
calcPi1 x = sum (take x [a / b | (a, b) <- zip numerators denoms])
  where
    numerators = cycle [4, -4]
    denoms = [1, 3 ..]

calcPi2 x = 3 + sum (take (x - 1) [a / b | (a, b) <- zip numerators denoms])
  where
    numerators = cycle [4, -4]
    denoms = [p * (p + 1) * (p + 2) | p <- [2, 4 ..]]