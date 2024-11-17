import           Data.List (sort)

main :: IO ()
main = do
    input <- getLine
    kanner <- getLine
    case smallest (zip [1..read input] (sort [read x | x <- words kanner])) of
        Nothing -> putStrLn "impossible"
        Just x  -> print x

smallest :: [(Float, Float)] -> Maybe Float
smallest [] = Just 1
smallest ((b, k):xs)
    | k/b > 1 = Nothing
    | otherwise = case smallest xs of
        Nothing -> Nothing
        Just x  -> Just (min (k/b) x)
