import Data.Char

forte :: String -> Bool
forte s | length s < 8 || null lstLetterM || null lstLetterm || null lstDigit = False
	| otherwise = True
	where lstLetterM = [ c | c <- s, (isLetter c && isUpper c)]
	      lstLetterm = [ c | c <- s, (isLetter c && isLower c)]
	      lstDigit = [ c | c <- s, isDigit c ]
