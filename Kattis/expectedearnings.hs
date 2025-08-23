main :: IO ()
main = do
    n:k:p:_ <- fmap (map read . words) getLine
    if n >= k / p
        then putStrLn "spela inte!"
        else putStrLn "spela"
