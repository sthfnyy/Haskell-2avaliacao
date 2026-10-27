somatorio :: Int -> Int
somatorio 0 = 0
somatorio numero = numero + somatorio (numero - 1)


main :: IO ()
main = do
    putStrLn "Digite um numero:"
    entrada <- getLine

    let numero = read entrada :: Int
    let resultado = somatorio numero

    putStrLn ("Resultado: " ++ show resultado)
