fibs :: [Integer]
fibs = gerarFibonacci 0 1

gerarFibonacci :: Integer -> Integer -> [Integer]
gerarFibonacci atual proximo =
    atual : gerarFibonacci proximo (atual + proximo)

nFibs :: Int -> [Integer]
nFibs quantidade = pegar quantidade fibs

pegar :: Int -> [Integer] -> [Integer]
pegar quantidade lista
  | quantidade <= 0 = []
pegar quantidade [] = []
pegar quantidade (primeiro:restante) =
    primeiro : pegar (quantidade - 1) restante


main :: IO ()
main = do
    putStrLn "Digite a quantidade de termos:"
    entrada <- getLine

    let quantidade = read entrada :: Int
    let resultado = nFibs quantidade

    putStrLn ("Fibonacci: " ++ show resultado)


