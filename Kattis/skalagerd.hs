main :: IO ()
main = do
    r <- fmap read getLine
    print (sqrt (r^2 + r^2))
