
uniao :: [Int] -> [Int] -> [Int]
uniao lista1 lista2 = semRepetidos (lista1 ++ lista2)

semRepetidos :: [Int] -> [Int]
semRepetidos [] = []
semRepetidos (primeiro:restante) =
    primeiro : semRepetidos (remover primeiro restante)

remover :: Int -> [Int] -> [Int]
remover numero [] = []
remover numero (primeiro:restante)
  | numero == primeiro = remover numero restante
  | otherwise = primeiro : remover numero restante


main :: IO ()
main = do
    putStrLn "Digite a primeira lista:"
    entrada1 <- getLine

    putStrLn "Digite a segunda lista:"
    entrada2 <- getLine

    let lista1 = read entrada1 :: [Int]
    let lista2 = read entrada2 :: [Int]
    let resultado = uniao lista1 lista2

    putStrLn ("Uniao das listas: " ++ show resultado)
