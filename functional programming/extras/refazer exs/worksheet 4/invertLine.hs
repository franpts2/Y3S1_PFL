main :: IO ()
main = do txt <- getContents
	  let invertedLinesList = map reverse (lines txt)
	  let allTogether = unlines invertedLinesList
	  let final = reverse (tail (reverse allTogether)) -- without the last \n
	  putStrLn (final)
	      	
