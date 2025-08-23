{-# LANGUAGE MultiWayIf #-}

main :: IO ()
main = do
    players <- fmap (tail . lines) getContents
    putStrLn $ if
        | and (zipWith (<=) players (tail players)) -> "INCREASING"
        | and (zipWith (>=) players (tail players)) -> "DECREASING"
        | otherwise                                 -> "NEITHER"
