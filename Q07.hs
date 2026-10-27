
maiorString :: String -> String -> String
maiorString texto1 texto2
  | tamanho texto1 >= tamanho texto2 = texto1
  | otherwise = texto2

tamanho :: String -> Int
tamanho [] = 0
tamanho (primeiro:restante) = 1 + tamanho restante


main :: IO ()
main = do
    putStrLn "Digite o primeiro texto:"
    texto1 <- getLine

    putStrLn "Digite o segundo texto:"
    texto2 <- getLine

    let resultado = maiorString texto1 texto2

    putStrLn ("Maior texto: " ++ resultado)
