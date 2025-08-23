main :: IO ()
main = do
    n <- fmap read getLine
    putStrLn $ if n `mod` 10 == 0
        then "Jebb"
        else "Neibb"
