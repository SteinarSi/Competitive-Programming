main :: IO ()
main = do
    [b, br, bs, a, as] <- fmap (map read . words) getLine
    print $ head [ a + i | i <- [0..], i*as > (br-b) * bs]
