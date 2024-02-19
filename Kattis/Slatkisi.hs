main :: IO ()
main = do
    [c,k] <- fmap (map read . words) getLine
    print (round (fromIntegral c / (10^k)) * (10^k))
