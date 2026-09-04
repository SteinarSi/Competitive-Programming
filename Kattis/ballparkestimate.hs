main :: IO ()
main = do
    n <- read <$> getLine
    let l = 10 ^ fromIntegral (floor (logBase 10 n))
    print (l * round (n / fromIntegral l))
