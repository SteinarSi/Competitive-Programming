main :: IO ()
main = do
    [n,k] <- fmap (map read . words) getContents
    print ((n+k-1) `div` k)
