import Text.Read (Lexeme(String))
type Dict = [String]

readDict :: IO Dict
readDict = do 
            txt <- readFile "/usr/share/dict/words"
            return (words txt)

reverseVideo :: String
reverseVideo = "\ESC[7m"

normalVideo :: String
normalVideo = "\ESC[0m"

-- a)
main1 :: IO()
main1 = do 
            dict <- readDict
            print (length dict)

-- b)
checkWord :: Dict -> String -> String
checkWord dict w | w `elem` dict = w
                 | otherwise = reverseVideo ++ w ++ normalVideo
            
-- c)
spellCheck :: Dict -> String -> String
spellCheck dict inputTxt = -- ex: ["Hello", "world", "This", "is"], Hello world\nThis is bad
    let
        textLines :: [String]
        textLines = lines inputTxt -- ["Hello world", "This is bad"]

        allWordsByLine :: [[String]]
        allWordsByLine = map words textLines -- [["Hello", "world"], ["This", "is", "bad"]]

        checkedWordsByLine :: [[String]]
        checkedWordsByLine = map (map (checkWord dict)) allWordsByLine -- [["Hello", "world"], ["This", "is", "\ESC[7mbad\ESC[0m"]]

        checkedLines :: [String]
        checkedLines = map unwords checkedWordsByLine -- [["Hello world"], ["This is \ESC[7mbad\ESC[0m"]]
    
        finalTxt :: String
        finalTxt = unlines checkedLines -- Hello world\nThis is \ESC[7mbad\ESC[0m 
    in 
        finalTxt

-- d)
main2 :: IO()
main2 = do
            dict <- readDict
            inputTxt <- getContents
            putStr(spellCheck dict inputTxt)

