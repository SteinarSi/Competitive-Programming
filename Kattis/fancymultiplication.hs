main :: IO ()
main = do
    x <- fmap read getLine
    y <- fmap read getLine
    print (x*y)
