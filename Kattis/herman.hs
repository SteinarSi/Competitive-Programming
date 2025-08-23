main :: IO ()
main = do
    r <- fmap read getLine
    print (r*r*pi)
    print ((2*r)^2 / 2)
