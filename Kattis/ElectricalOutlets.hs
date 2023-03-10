main = do
    input <- getLine
    inputs <- getInputs (read input)
    handleInputs inputs

handleInputs :: [[Int]] -> IO ()
handleInputs [] = return ()
handleInputs (x:xs) = do
    print (1 + sum (tail x))
    handleInputs xs

getInputs :: Int -> IO [[Int]]
getInputs 0 = return []
getInputs n = do
    x <- getLine
    xs <- getInputs (n-1)
    return ([ (read y)-1 | y<-words x]:xs)