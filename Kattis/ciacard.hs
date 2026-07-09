{-# LANGUAGE MultiWayIf #-}

main :: IO ()
main = do
    c <- read <$> getLine

    print $ if
        | c >= 16000 -> 300
        | c >= 8000  -> 150 + 25 * ((c-8000) `div` 2000)
        | otherwise  -> 25 * (c `div` 2000)
