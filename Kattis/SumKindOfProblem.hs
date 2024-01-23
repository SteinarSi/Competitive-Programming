import           Control.Monad (replicateM_)

main :: IO ()
main = do
    p <- fmap read getLine
    replicateM_ p $ do
        [k, n] <- fmap (map read . words) getLine
        let both = sumAll (n*2)
            odds = (both - n) `div` 2
            evens = both - odds
        putStrLn . unwords $ map show [k, sumAll n, odds, evens]

sumAll :: Integer -> Integer
sumAll n = n * (n+1) `div` 2
