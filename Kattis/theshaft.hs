main :: IO ()
main = do
    [g,c,t] <- map read . words <$> getLine

    let
        dist :: Double -> Double
        dist x = (g/2)*x^2

        bin :: Double -> Double -> Double
        bin lo hi
            | hi-lo <= 0.000001 = mi
            | otherwise = case compare (mi + dist mi / c) t of
                EQ -> mi
                LT -> bin mi hi
                GT -> bin lo mi
          where
            mi = (lo+hi) / 2

    print (dist (bin 0 t))
