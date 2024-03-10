main :: IO ()
main = getLine >>= putStrLn . solve

solve :: String -> String
solve [] = []
solve [x] = [x]
solve (x:y:xs) | x == y    = solve (y:xs)
               | otherwise = x : solve (y:xs)
