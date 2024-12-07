main :: IO ()
main = do
    n <- fmap read getLine

    print $ binSearch 30 (log n) 0 10

binSearch :: Int -> Double -> Double -> Double -> Double
binSearch 0 _ lo hi = (lo+hi) / 2
binSearch i n lo hi | mid * log mid > n = binSearch (i-1) n lo mid
                    | otherwise         = binSearch (i-1) n mid hi
    where mid = (lo+hi) / 2
