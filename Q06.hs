inverter :: String -> String
inverter [] = []
inverter (letra:restante) = inverter restante ++ [letra]

main :: IO ()
main = do
    putStrLn "Digite uma palavra:"
    texto <- getLine

    let resultado = inverter texto

    putStrLn ("Texto invertido: " ++ resultado)