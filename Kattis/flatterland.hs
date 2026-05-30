main :: IO ()
main = do
    [n,x] <- fmap (map read . words) getContents
    print ((n-1) * x)
