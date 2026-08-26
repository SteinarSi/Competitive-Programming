main :: IO ()
main = do
    [c,n] <- map (read @Int) . words <$> getLine
    print $ case compare c n of
        LT -> c+1
        EQ -> c
        GT -> 0
