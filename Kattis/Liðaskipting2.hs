main :: IO ()
main = do
    n <- fmap read getLine
    print n
    print ((n+2) `div` 3)
