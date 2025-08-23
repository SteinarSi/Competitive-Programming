main :: IO ()
main = do
    n <- fmap read getLine
    putStrLn $ if even (n `div` 2)
        then "No"
        else "Yes"
