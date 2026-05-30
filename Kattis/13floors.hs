main :: IO ()
main = do
    x <- read <$> getLine
    print $ if x <= 12
        then x
        else succ x
