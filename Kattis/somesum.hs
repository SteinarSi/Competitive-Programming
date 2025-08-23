{-# LANGUAGE MultiWayIf #-}

main :: IO ()
main = do
    c <- fmap (combs . read) getLine
    putStrLn $ if
        | all odd  c -> "Odd"
        | all even c -> "Even"
        | otherwise  -> "Either"

combs :: Int -> [Int]
combs n = do
    i <- [1..100]
    let j = i + n - 1
    pure $ sum [i..j]
