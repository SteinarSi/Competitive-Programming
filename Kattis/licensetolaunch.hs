main = do
    antall <- getLine
    input <- getLine
    let tall = zip [read n::Int | n<-(words input)] [0..]
    putStrLn (show (findLowest tall (head tall)))

findLowest [] (a, b) = b
findLowest ((t, i):xs) (a, b)
    | t < a = findLowest xs (t, i)
    | otherwise = findLowest xs (a, b)