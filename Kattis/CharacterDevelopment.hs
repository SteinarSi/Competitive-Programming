main :: IO ()
main = getLine >>= print . sum . flip map [2..30] . choose . read

choose :: Integer -> Integer -> Integer
choose n k | k > n           = 0
           | k == 0          = 1
           | k > (n `div` 2) = n `choose` (n-k)
           | otherwise       = n * ((n-1) `choose` (k-1)) `div` k
