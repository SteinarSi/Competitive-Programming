main :: IO ()
main = do
    [v, a, t] <- fmap (map read . words) getLine
    print (v*t + a*t**2 / 2)
