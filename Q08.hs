tabuada :: Int -> [(Int, Int, Int)]
tabuada numero = gerarTabuada numero 1

gerarTabuada :: Int -> Int -> [(Int, Int, Int)]
gerarTabuada numero contador
  | contador > 10 = []
  | otherwise = (numero, contador, numero * contador) :
                gerarTabuada numero (contador + 1)


main :: IO ()
main = do
    putStrLn "Digite um numero para a tabuada:"
    entrada <- getLine

    let numero = read entrada :: Int
    let resultado = tabuada numero

    putStrLn ("Tabuada: " ++ show resultado)
