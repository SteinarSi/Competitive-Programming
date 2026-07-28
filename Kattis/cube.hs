main :: IO ()
main = do
    x <- fmap read getLine
    print (x^3)
