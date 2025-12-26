import Data.Char

main :: IO ()
main = do txt <- getContents
	  putStrLn (reverse (tail (reverse (map rot13 txt))))
	  
rot13 :: Char -> Char
rot13 c | isUpper c = rot13Upper c
	| isLower c = rot13Lower c
	| otherwise = c 

rot13Upper :: Char -> Char
rot13Upper c = chr (shifted + ord 'A')
	where normalized = ord c - ord 'A'
	      shifted = (normalized + 13) `mod` 26

rot13Lower :: Char -> Char
rot13Lower c = chr (shifted + ord 'a')
	where normalized = ord c - ord 'a'
	      shifted = (normalized + 13) `mod` 26
