classifica :: Float -> Float -> String
classifica p h 
    | imc < 18.5 = "baixo peso"
    | imc >= 18.5 && imc < 25 = "peso normal"
    | imc >= 25 && imc < 30 = "excesso de peso"
    | imc >= 30 = "obesidade"
    | otherwise = error "imc negativo"
    where imc = p/(h**2)