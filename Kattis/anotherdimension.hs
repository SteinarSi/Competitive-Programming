main :: IO ()
main = do
    d <- fmap read getLine
    print (pi * (d/2)^3 * 2 / 3)
