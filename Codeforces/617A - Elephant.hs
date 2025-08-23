main :: IO ()
main = do
    x <- read <$> getLine

    print ((x+4) `div` 5)
