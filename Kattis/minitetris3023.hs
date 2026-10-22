main :: IO ()
main = do
    [a,b,c] <- map read . words <$> getLine

    print $ if c >= 2
        then 2*a + 2*b + 3 + 3 * ((c-2) `div` 2)
        else 2*a + 3 * (c `div` 2)
