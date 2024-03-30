main :: IO ()
main = getLine >>= print . collatz . read

collatz :: Int -> Int
collatz 1 = 1
collatz x | even x    = x + collatz (x `div` 2)
          | otherwise = x + collatz (3 * x + 1)
