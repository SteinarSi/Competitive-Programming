main = do
    input <- getLine
    let o = length input - 1
    putStrLn ("AH" ++ (replicate o 'O') ++ "W")