main :: IO ()
main = do
    [d,h] <- map read . words <$> getLine
    putStrLn $ if atan (h/d) * 360 / (2*pi) <= 15
        then "Yes"
        else "No"
