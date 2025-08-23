main :: IO ()
main = do
    [a,b,c] <- fmap (map read . words) getContents
    print $ case compare (b*b) (4*a*c) of
        LT -> 0
        EQ -> 1
        GT -> 2
