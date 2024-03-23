main :: IO ()
main = do
    n <- fmap read getLine
    putStrLn $ concat [
            "0.",
            replicate (n-1) '0',
            "1"
        ]
