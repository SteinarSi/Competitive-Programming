main :: IO ()
main = do
    n <- fmap read getLine
    print ((n `div` 2 + 1) * ((n+1) `div` 2 + 1))
