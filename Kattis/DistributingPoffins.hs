main :: IO ()
main = do
    [n,m] <- fmap (map read . words) getLine
    print $ if m `mod` n == 0
        then 0
        else 1
