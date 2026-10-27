numerosInfinitos :: Integer -> [Integer]
numerosInfinitos numero =
    numero : numerosInfinitos (numero + 1)

procurarNumero :: Integer -> Integer
procurarNumero numero =
    procurarLista (numerosInfinitos (numero + 1))

procurarLista :: [Integer] -> Integer
procurarLista (primeiro:restante)
  | mod primeiro 7 == 0 && mod primeiro 10 == 3 = primeiro
  | otherwise = procurarLista restante


main :: IO ()
main = do
    putStrLn "Digite um numero:"
    entrada <- getLine

    let numero = read entrada :: Integer
    let resultado = procurarNumero numero

    putStrLn ("Resultado: " ++ show resultado)
