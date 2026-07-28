{-# LANGUAGE MultiWayIf #-}

main :: IO ()
main = do
    r <- read <$> getLine

    putStrLn if
        | r >= 2700 -> "Super grandmaster"
        | r >= 2500 -> "Grandmaster"
        | r >= 2400 -> "International grandmaster"
        | r >= 1000 -> "Amateur"
        | otherwise -> "Invalid"
