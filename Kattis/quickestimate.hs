main = do
    antall <- getLine
    getNumberOfDigits (read antall::Int)

getNumberOfDigits :: Int -> IO()
getNumberOfDigits n
    | n == 0 = return ()
    | otherwise = do
        input <- getLine
        print (length input)
        getNumberOfDigits (n-1)