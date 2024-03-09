main :: IO ()
main = do
    a <- fmap read getLine
    b <- fmap read getLine
    print (min a b * 2)
