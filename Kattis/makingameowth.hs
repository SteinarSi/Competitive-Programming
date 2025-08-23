main :: IO ()
main = do
    [n, p, x, y] <- fmap (map read . words) getLine
    print (p * x + y * (p `div` (n-1)))
