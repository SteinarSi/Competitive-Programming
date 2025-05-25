main :: IO ()
main = do
    [n,m] <- fmap (map read . words) getContents

    let missing = n*4 - m

    putStrLn $ if missing < 0 || odd missing || 2*n > m
        then "Rong talning"
        else show (n - missing `div` 2)
