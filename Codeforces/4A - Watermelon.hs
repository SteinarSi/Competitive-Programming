main :: IO ()
main = do
    n <- fmap read getLine
    putStrLn $ if even n && n >= 4
        then "Yes"
        else "No"
