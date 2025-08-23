main :: IO ()
main = do
    n <- fmap read getLine
    print (n `div` 2)
