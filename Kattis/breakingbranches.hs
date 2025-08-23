main :: IO ()
main = do
    n <- fmap read getLine
    putStrLn $ if even n
        then "Alice\n1"
        else "Bob"
