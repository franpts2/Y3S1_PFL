classifyBMI :: Float -> Float -> String
classifyBMI w h | bmi < 18.5 = "underweight"
                | (bmi>=18.5) && (bmi<25) = "normal weight"
                | (bmi>=25) && (bmi<30) = "overweight"
                | bmi>=30 = "obese"
        where bmi = w/(h^2)