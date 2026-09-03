main :: IO ()
main = do
    n <- read <$> getLine
    print $ if odd n
        then 3
        else 2
