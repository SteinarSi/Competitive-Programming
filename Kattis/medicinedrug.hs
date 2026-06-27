main :: IO ()
main = do
    [n,k,l] <- fmap (map read . words) getLine
    print ((n*k+l-1) `div` l)
