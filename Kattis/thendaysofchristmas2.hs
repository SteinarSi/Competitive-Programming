main :: IO ()
main = do
    n <- read <$> getLine
    print (((n*(n+1)*(n+2)) `div` 6) `mod` 998244353)
