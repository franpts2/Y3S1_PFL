import Data.Char

-- a)
type Aword = String
type Line = [Aword]
type Paragraph = [Line]

fillWords :: Int -> [Aword] -> Paragraph
fillWords _ [] = []
fillWords 0 _ = [[]]
fillWords n ls = currLine : fillWords n restOfWords
    where (currLine, restOfWords) = gatherLine n [] 0 ls

-- max width, accumulator, cur length, remaining input words to check, (words that fit, rest of words)
gatherLine :: Int -> [Aword] -> Int -> [Aword] -> (Line, [Aword])
gatherLine _ acc _ [] = (reverse acc, [])
gatherLine n acc len (w:ws) 
        | null acc && length w <= n = gatherLine n (w:acc) (length w) ws 
        | potentialLen <= n = gatherLine n (w:acc) potentialLen ws
        | otherwise = (reverse acc, (w:ws))
    where 
        potentialLen = len + 1 + length w

-- b)
main :: IO()
main = do txt <- getLine
          print (fillWords 70 (words txt))
