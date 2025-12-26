classify1 :: Int -> String
classify1 x = if x<=9 then "failed" else
                (if (x>=10 && x<=12) then "passed" else
                    (if (x>=13 && x<=15) then "good" else (
                        if (x>=16 && x<=18) then "very good" else (
                            if (x>=19 && x<=20) then "excellent" else "incorrect grade"
                        )
                    ))
                )

classify2 :: Int -> String
classify2 x | x<=9 = "failed"
            | (x>=10 && x<=12) = "passed"
            | (x>=13 && x<=15) = "good"
            | (x>=16 && x<=18) = "very good"
            | (x>=19 && x<=20) = "excellent"
            | otherwise = "incorrect grade"

{-
classify3 :: Int -> String
classify3 x = case x of 
                x<=9 -> "failed" 
-}