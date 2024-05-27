import Data.Char (isDigit)

-- Define a function to split the input into (fecha, rest)
splitFechaRest :: String -> (String, String)
splitFechaRest id = splitAt 3 id

-- Define a function to split the rest into (prog, n)
splitProgN :: String -> (String, String)
splitProgN rest = splitAt 2 rest

-- Define a function to construct the semestre string
makeSemestre :: String -> String
makeSemestre fecha = "20" ++ take 2 fecha ++ "-" ++ drop 2 fecha

-- Define a function to construct the admision string
makeAdmision :: String -> String
makeAdmision n = 
    let nInt = read n :: Int
    in "num" ++ show nInt ++ if even nInt then " even" else " odd"

-- Define a function to determine the category
determineCategoria :: String -> String
determineCategoria prog = 
    let progInt = read prog :: Int
        factores = [num | num <- [1..progInt-1], progInt `mod` num == 0]
        sumaFactores = sum factores
    in if progInt == sumaFactores
       then "Engineering"
       else if progInt > sumaFactores
            then "Humanities"
            else "Administrative"

-- ProcessID function
processID :: String -> String
processID id = 
    let (fecha, rest) = splitFechaRest id
        (prog, n) = splitProgN rest
        semestre = makeSemestre fecha
        admision = makeAdmision n
        categoria = determineCategoria prog
    in semestre ++ " " ++ categoria ++ " " ++ admision ++ "\n"

-- Funcion principal (main)
main :: IO ()
main = do
    --putStrLn "Ingresa la id: "
    id <- getLine
    if length id == 8 && all isDigit id
        then putStrLn $ processID id
        else putStrLn "La id es invalida\n"
    --main

