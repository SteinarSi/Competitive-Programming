main :: IO ()
main = do
    [l, r] <- fmap (map read . words) getLine
    putStrLn $ if 2*(l/2)**2 <= r**2
        then "fits"
        else "nope"
