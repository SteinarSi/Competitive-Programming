main = do
    dager <- getLine
    input <- getLine
    putStrLn (show (length [x | x<-words input, (head x == '-')]))