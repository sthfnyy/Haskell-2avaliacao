multiplos :: [Int] -> Int -> [Int]
multiplos [] numero = []
multiplos (primeiro:restante) numero
  | mod primeiro 3 == 0 && primeiro > numero =
      primeiro : multiplos restante numero
  | otherwise = multiplos restante numero



main :: IO ()
main = do
    putStrLn "Digite uma lista de numeros:"
    entradaLista <- getLine

    putStrLn "Digite o numero limite:"
    entradaNumero <- getLine

    let lista = read entradaLista :: [Int]
    let numero = read entradaNumero :: Int
    let resultado = multiplos lista numero

    putStrLn ("Resultado: " ++ show resultado)
