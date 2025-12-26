safetail1 :: [a] -> [a]
safetail1 []     = []
safetail1 (_:xs) = xs

safetail2 :: [a] -> [a]
safetail2 l | null l    = []
            | otherwise = let (_:xs) = l in xs

safetail3 :: [a] -> [a]
safetail3 l = if null l then [] else (
        let (_:xs) = l in xs
    )