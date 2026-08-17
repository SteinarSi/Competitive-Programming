{-# LANGUAGE MultiWayIf #-}

main :: IO ()
main = do
    n <- read <$> getLine
    putStrLn if
        | n <= 1    -> "impossible"
        | odd n     -> '7' : replicate ((n-3) `div` 2) '1'
        | otherwise -> replicate (n `div` 2) '1'
