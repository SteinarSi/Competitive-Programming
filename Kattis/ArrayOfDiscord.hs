main = do
    antall <- getLine
    tall <- getLine
    let tallls = [read x | x<- words tall]
    case unSort tallls [] of
        Nothing -> putStrLn "impossible"
        Just xs -> mapM_ (\x -> putStr (show x ++ " ")) xs

unSort :: [Integer] -> [Integer] -> Maybe [Integer]
unSort [] akk = Nothing
unSort [x] all = Nothing
unSort (x:y:xs) akk = if high > y then Just (akk ++ [high, y] ++ xs)
                  else if x > low then Just (akk ++ [x, low] ++ xs)
                  else unSort (y:xs) (akk ++ [x])
    where 
        high = read $ higher (show x)
        low  = if length (show y) == 1 then 0 else read $ lower (show y)

lower :: String -> String
lower "" = ""
lower ('1':xs) = '1' : low' xs
lower (xs) = low' xs

low' :: String -> String
low' "" = ""
low' ('1':xs) = '1' : low' xs
low' (x:xs) = '1' : xs

higher :: String -> String
higher "" = ""
higher ('9':xs) = '9' : higher xs
higher (x:xs) = '9':xs