main :: IO ()
main = do
    [a,b,c] <- fmap (map (read :: String -> Int) . words) getLine
    putStrLn (solve a b c)

solve :: Int -> Int -> Int -> String
solve a b c | b < a && b < c || b > a && b > c = "turned"
            | abs (b-a) < abs (c-b) = "accelerated"
            | abs (b-a) > abs (c-b) = "braked"
            | otherwise = "cruised"
