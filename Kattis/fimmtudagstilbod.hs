main :: IO ()
main = do
    year <- fmap read getLine
    print $ if year <= 2020
        then 1000
        else 1000 + (year - 2020) * 100
