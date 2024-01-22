main :: IO ()
main = do
    [a, b] <- fmap (map read . words) getLine
    print (gcd a b)
