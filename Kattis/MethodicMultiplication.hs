main = do
    a <- getLine
    b <-getLine
    let result = countS a * countS b
    putStrLn ((concat $ replicate result "S(") ++ "0" ++ (concat $ replicate result ")"))

countS :: String -> Int
countS "" = 0
countS ('S':xs) = 1 + countS xs
countS ('0':xs) = 0
countS (x:xs) = countS xs