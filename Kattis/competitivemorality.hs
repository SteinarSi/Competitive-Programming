main :: IO ()
main = do
    [r,b] <- fmap (map read . words) getContents
    putStrLn $ if b+1 >= r
        then "BLUE"
        else "RED"
