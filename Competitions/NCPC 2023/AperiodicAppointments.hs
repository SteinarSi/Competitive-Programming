{-# LANGUAGE Strict #-}

main :: IO ()
main = do
    n:k:_ <- fmap (map read . words) getLine
    print $ solve n k

solve :: Integer -> Integer -> Integer
solve n k | n < k*k = n `div` (k+1)
          | tots k <= n = ones k + n - tots k
          | otherwise   = block 0 n k
    where
        block :: Integer -> Integer -> Integer -> Integer
        block s r p | tots 1 > r  = s
                    | otherwise   = let i = bin r (1, p)
                                        amount = r `div` tots i
                                    in  block (s + amount * ones i) (r-amount * tots i) (i-1)

        bin :: Integer -> (Integer, Integer) -> Integer
        bin r (lo, hi) | tots guess <= r && tots (guess + 1) > r = guess
                       | tots guess <= r                         = bin r (guess+1, hi)
                       | otherwise                               = bin r (lo, guess-1)
            where guess = (lo + hi) `div` 2

        tots p = ones (p+1)
        ones p = (k^p-1) `div` (k-1)
