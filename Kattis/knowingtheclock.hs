main :: IO ()
main = do
    [h,m] <- map read . words <$> getLine
    putStrLn $ if (h `mod` 30) * 12 == m
        then "yes"
        else "no"
