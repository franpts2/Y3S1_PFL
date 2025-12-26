main :: IO ()
main = do txt <- getContents
	  putStrLn (show (length (lines txt)))
	  putStrLn (show (length (words txt)))
	  putStrLn (show (length txt))

