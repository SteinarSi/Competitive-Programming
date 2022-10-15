main :: IO ()
main = do
    con <- getContents
    let lin = lines con
    analyze lin 1

analyze :: [String] -> Int -> IO ()
analyze [] _ = return ()
analyze (x:xs) i = do
    let tall = [read y | y<-tail $ words x]
        ma = maximum tall
        mi = minimum tall
        ra = ma - mi
    putStrLn ("Case " ++ show i ++ ": " ++ show mi ++ " " ++ show ma ++ " " ++  show ra)
    analyze xs (i+1)