
mdc :: Int -> Int -> Int
mdc numero1 numero2
  | numero2 == 0 = abs numero1
  | otherwise = mdc numero2 (mod numero1 numero2)


main :: IO ()
main = do
    putStrLn "Digite o primeiro numero:"
    entrada1 <- getLine

    putStrLn "Digite o segundo numero:"
    entrada2 <- getLine

    let numero1 = read entrada1 :: Int
    let numero2 = read entrada2 :: Int
    let resultado = mdc numero1 numero2

    putStrLn ("MDC: " ++ show resultado)
