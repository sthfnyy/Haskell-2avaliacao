primos :: [Integer]
primos = gerarPrimos 2

gerarPrimos :: Integer -> [Integer]
gerarPrimos numero
  | ehPrimo numero = numero : gerarPrimos (numero + 1)
  | otherwise = gerarPrimos (numero + 1)

ehPrimo :: Integer -> Bool
ehPrimo numero
  | numero < 2 = False
  | otherwise = verificarDivisor numero 2

verificarDivisor :: Integer -> Integer -> Bool
verificarDivisor numero divisor
  | divisor * divisor > numero = True
  | mod numero divisor == 0 = False
  | otherwise = verificarDivisor numero (divisor + 1)

nesimoPrimo :: Int -> Integer
nesimoPrimo posicao = buscarPosicao posicao primos

buscarPosicao :: Int -> [Integer] -> Integer
buscarPosicao 1 (primeiro:restante) = primeiro
buscarPosicao posicao (primeiro:restante) =
    buscarPosicao (posicao - 1) restante



main :: IO ()
main = do
    putStrLn "Digite a posicao do numero primo:"
    entrada <- getLine

    let posicao = read entrada :: Int
    let resultado = nesimoPrimo posicao

    putStrLn ("Numero primo: " ++ show resultado)
