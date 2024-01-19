main :: IO ()
main = do
    [d, m] <- fmap (map (pred . read) . words) getLine
    putStrLn (days !! ((months !! m + d) `mod` 7))

days :: [String]
days = ["Thursday", "Friday", "Saturday", "Sunday", "Monday", "Tuesday", "Wednesday"]

months :: [Int]
months = scanl (+) 0 [31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31]
