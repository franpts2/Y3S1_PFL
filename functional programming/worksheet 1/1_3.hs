-- a)
second :: [Int] -> Int
second a = head (tail a)

-- b)
last1 :: [Int] -> Int
last1 a = head (reverse a)

last2 :: [Int] -> Int
last2 a = head (drop (length a - 1) a)

-- c)
init1 :: [Int] -> [Int]
init1 a = reverse (tail (reverse a))

init2 :: [Int] -> [Int]
init2 a = take (length a - 1) a

-- d)
middle' :: [Int] -> Int
middle' a = head (drop (div (length a) 2) a)

-- e)
checkPalindrome :: String -> Bool
checkPalindrome a = (a == reverse a)