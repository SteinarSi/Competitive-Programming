main :: IO ()
main = do
    n <- fmap read getLine
    a <- getLine
    b <- getLine
    putStrLn $ if a == if odd n then flop b else b
        then "Deletion succeeded"
        else "Deletion failed"

flop :: String -> String
flop "" = ""
flop ('0':xs) = '1' : flop xs
flop ('1':xs) = '0' : flop xs
