main = do
    findBest 1 0 0

findBest :: Int -> Int -> Int -> IO ()
findBest n bestesum bestekar
    | n == 6 = putStrLn (show bestekar ++ " " ++ show bestesum)
    | otherwise = do
        summ <- takeInput
        if summ < bestesum then findBest (n+1) bestesum bestekar
        else findBest (n+1) summ n

takeInput :: IO Int
takeInput = do
    input <- getLine
    let tall = words input
    return (sum [read n::Int | n <- tall])