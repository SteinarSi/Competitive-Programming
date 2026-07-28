main :: IO ()
main = do
    f <- fmap read getLine
    print (round ((f-32) / 1.8))
