main :: IO ()
main = do
    n <- read <$> getLine
    let u = floor (sqrt (fromIntegral n))
    print (2 * sum (map (\k -> n `div` k) [1..u]) - u^2)
