main :: IO ()
main = do
    [h,w,l,c] <- fmap (map read . words) getLine

    putStrLn $ case compare c (h*w*l) of
        LT -> "SO MUCH SPACE"
        EQ -> "COZY"
        GT -> "TOO TIGHT"
