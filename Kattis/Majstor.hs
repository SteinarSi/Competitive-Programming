main = do
    rounds <- getLine
    sven <- getLine
    friends <- getLine
    frinputs <- getInputs (read friends)
    print (actualSum sven frinputs (read rounds))
    print (hypothetical frinputs (read rounds))

getInputs :: Int -> IO [String]
getInputs 0 = return []
getInputs n = do
    x <- getLine
    xs <- getInputs (n-1)
    return (x:xs)

hypothetical :: [String] -> Int -> Int
hypothetical frinputs rounds = sum (map (\r -> maximum (map (\s-> round' s (map (!!r) frinputs)) "RPS")) [0..rounds-1])

actualSum :: String -> [String] -> Int -> Int
actualSum sven frinputs rounds = sum (map (\i-> round' (sven!!i) (map (!!i) frinputs)) [0..rounds-1]) 

round' :: Char -> [Char] -> Int
round' s frinp = sum (map (\fr -> headtohead s fr) frinp)

headtohead :: Char -> Char -> Int
headtohead 'S' 'S' = 1
headtohead 'S' 'P' = 2
headtohead 'S' 'R' = 0
headtohead 'P' 'P' = 1
headtohead 'P' 'S' = 0
headtohead 'P' 'R' = 2
headtohead 'R' 'R' = 1
headtohead 'R' 'P' = 0
headtohead 'R' 'S' = 2
headtohead _ _ = error "bruh"