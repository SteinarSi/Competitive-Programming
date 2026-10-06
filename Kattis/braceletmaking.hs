main :: IO ()
main = do
    [b,g] <- map read . words <$> getLine
    let t = b+g
    print $ if b == 0 || g == 0
        then 1
        else (choose t b + t * choose (t `div` 2) (b `div` 2)) `div` (2*t)

choose :: Int -> Int -> Int
choose n k
    | k > n           = 0
    | k == 0          = 1
    | k > (n `div` 2) = n `choose` (n-k)
    | otherwise       = n * ((n-1) `choose` (k-1)) `div` k
