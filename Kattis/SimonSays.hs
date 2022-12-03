main = do
    antall <- getLine
    let n = read antall::Int
    handleInputs n

handleInputs :: Int -> IO()
handleInputs n 
    | n == 0 = return ()
    | otherwise = do
        input <- getLine
        if (take 10 input) == "Simon says" then do
            putStrLn (drop 11 input)
            handleInputs (n-1)
        else handleInputs (n-1)