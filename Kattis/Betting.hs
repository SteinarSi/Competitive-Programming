main :: IO ()
main = do
    inn <- fmap read getLine
    print (100 / inn)
    print (100 / (100-inn))
