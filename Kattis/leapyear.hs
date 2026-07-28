main :: IO ()
main = do
    y <- fmap read getLine
    print (y `mod` 400 == 0 || (y `mod` 4 == 0 && y `mod` 100 /= 0))
