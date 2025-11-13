import Data.Char

main :: IO()
main = do 
        txt <- getLine
        putStr (rot13 txt)

rot13 :: String -> String
rot13 txt = (map rot13Char txt) ++ "\n"

rot13Char :: Char -> Char
rot13Char c | not (isLetter c) = c
            | isAsciiLower c = rot13CharLower c 
            | otherwise = rot13CharUpper c
            
rot13CharLower :: Char -> Char
rot13CharLower c = chr (shifted + ord 'a')
    where 
        normalized = ord c - ord 'a'
        shifted = (normalized + 13) `mod` 26

rot13CharUpper :: Char -> Char
rot13CharUpper c = chr (shifted + ord 'a')
    where 
        normalized = ord c - ord 'A'
        shifted = (normalized + 13) `mod` 26