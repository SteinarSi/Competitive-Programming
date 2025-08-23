main :: IO ()
main = do
    [d,w,n] <- fmap (map read . words) getContents
    putStrLn $ if pi*d >= w*n
        then "YES"
        else "NO"
