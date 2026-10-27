comuns :: [Int] -> [Int] -> [Int]
comuns [] lista2 = []
comuns lista1 [] = []

comuns (primeiro1:restante1) (primeiro2:restante2)
  | primeiro1 == primeiro2 =
      primeiro1 : comuns (removerIguais primeiro1 restante1) (removerIguais primeiro2 restante2)
  | primeiro1 < primeiro2 =
      comuns restante1 (primeiro2:restante2)
  | otherwise =
      comuns (primeiro1:restante1) restante2

removerIguais :: Int -> [Int] -> [Int]
removerIguais numero [] = []
removerIguais numero (primeiro:restante)
  | numero == primeiro = removerIguais numero restante
  | otherwise = primeiro:restante


main :: IO ()
main = do
    putStrLn "Digite a primeira lista ordenada:"
    entrada1 <- getLine

    putStrLn "Digite a segunda lista ordenada:"
    entrada2 <- getLine

    let lista1 = read entrada1 :: [Int]
    let lista2 = read entrada2 :: [Int]
    let resultado = comuns lista1 lista2

    putStrLn ("Elementos em comum: " ++ show resultado)
