main :: IO ()
main = do
    n <- fmap read getLine
    print $ head $ filter isPrime [n-1,n-2..]

isPrime :: Int -> Bool
isPrime 2 = True
isPrime p = not (any ((0==) . mod p) [2..ceiling (sqrt (fromIntegral p))])
