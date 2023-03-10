
main :: IO ()
main = getLine >>= collatz . read

collatz :: Int -> IO ()
collatz n = putStr (show n ++ " ") >> next
    where next | n == 1    = putChar '\n'
               | even n    = collatz (n `div` 2)
               | otherwise = collatz (n*3 + 1)