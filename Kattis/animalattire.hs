{-# LANGUAGE MultiWayIf #-}

main :: IO ()
main = do
    [k,n] <- map read . words <$> getLine

    let u = upper 0 (n `div` 2)
        s = split (-1) (k+1)

        upper lo hi
            | hi-lo <= 1 = hi
            | mi^k >= n  = upper lo mi
            | otherwise  = upper mi hi
          where
            mi = (lo+hi) `div` 2

        split lo hi
            | hi-lo <= 1 = lo
            | total >= n = split mi hi
            | otherwise  = split lo mi
          where
            mi = (lo+hi) `div` 2
            total = (u-1)^mi * u^(k-mi)

    print $ if
        | n <= 1    -> k
        | k <= 1    -> n
        | otherwise -> s*(u-1) + (k-s)*u
