main :: IO ()
main = do
    [l, n] <- fmap (map read . words) getLine
    print (loo l n)

loo :: Int -> Int -> Int
loo _ 0 = 0
loo l n | r == 0    = 1
        | otherwise = 1 + loo l (n - r)
    where
        r = l `mod` n
