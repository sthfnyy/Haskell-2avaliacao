-- BASE DE DADOS FORNECIDA PELO PROFESSO
base :: Int -> (String, String, Int, Int)
base x
  | x == 1 = ("Brasil", "America do Sul", 5, 2002)
  | x == 2 = ("Alemanha", "Europa", 4, 2014)
  | x == 3 = ("Italia", "Europa", 4, 2006)
  | x == 4 = ("Argentina", "America do Sul", 3, 2022)
  | x == 5 = ("Franca", "Europa", 2, 2018)
  | x == 6 = ("Uruguai", "America do Sul", 2, 1950)
  | x == 7 = ("Inglaterra", "Europa", 1, 1966)
  | x == 8 = ("Espanha", "Europa", 1, 2010)
  | x == 9 = ("Holanda", "Europa", 0, 0)
  | x == 10 = ("Portugal", "Europa", 0, 0)
  | otherwise = ("", "", 0, 0)


-- QUESTAO 10 (A)
-- Contar quantas selecoes sao europeias

contarEuropeias :: Int -> Int
contarEuropeias indice
  | indice > 10 = 0
  | continente == "Europa" = 1 + contarEuropeias (indice + 1)
  | otherwise = contarEuropeias (indice + 1)
  where
    (nome, continente, titulos, ano) = base indice

europeias :: Int
europeias = contarEuropeias 1


-- QUESTAO 10 (B)
-- Listar selecoes campeas antes de 2000

campeasAntigas :: Int -> [String]
campeasAntigas indice
  | indice > 10 = []
  | titulos > 0 && ano < 2000 =
      nome : campeasAntigas (indice + 1)
  | otherwise = campeasAntigas (indice + 1)
  where
    (nome, continente, titulos, ano) = base indice

listaCampeas :: [String]
listaCampeas = campeasAntigas 1


-- QUESTAO 10 (C)
-- Encontrar a selecao com mais titulos mundiais

maiorCampea :: Int -> String -> Int -> String
maiorCampea indice melhorNome maiorTitulos
  | indice > 10 = melhorNome
  | titulos > maiorTitulos =
      maiorCampea (indice + 1) nome titulos
  | otherwise =
      maiorCampea (indice + 1) melhorNome maiorTitulos
  where
    (nome, continente, titulos, ano) = base indice

selecaoMaisTitulos :: String
selecaoMaisTitulos = maiorCampea 1 "" 0


-- QUESTAO 10 (D)
-- Somar a quantidade de titulos por continente

somarTitulos :: String -> Int -> Int
somarTitulos continenteProcurado indice
  | indice > 10 = 0
  | continente == continenteProcurado =
      titulos + somarTitulos continenteProcurado (indice + 1)
  | otherwise =
      somarTitulos continenteProcurado (indice + 1)
  where
    (nome, continente, titulos, ano) = base indice

titulosContinentes :: [(String, Int)]
titulosContinentes =
  [("America do Sul", somarTitulos "America do Sul" 1),
   ("Europa", somarTitulos "Europa" 1)]


-- Menu para executar as alternativas

main :: IO ()
main = do
    putStrLn "Selecione uma opcao:"
    putStrLn "1 - Quantidade de selecoes europeias"
    putStrLn "2 - Campeas antes de 2000"
    putStrLn "3 - Selecao com mais titulos"
    putStrLn "4 - Titulos por continente"

    entrada <- getLine

    let opcao = read entrada :: Int

    if opcao == 1 then
        putStrLn ("Quantidade de europeias: " ++ show europeias)
    else if opcao == 2 then
        putStrLn ("Campeas antes de 2000: " ++ show listaCampeas)
    else if opcao == 3 then
        putStrLn ("Selecao com mais titulos: " ++ selecaoMaisTitulos)
    else if opcao == 4 then
        putStrLn ("Titulos por continente: " ++ show titulosContinentes)
    else
        putStrLn "Opcao invalida"
