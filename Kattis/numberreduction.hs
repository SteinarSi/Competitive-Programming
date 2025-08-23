import           Control.Arrow ((>>>))

main :: IO ()
main = getLine >>= (read >>> reduce >>> print)

reduce :: Int -> Int
reduce 1 = 0
reduce x | even x    = 1 + reduce (x `div` 2)
         | otherwise = 1 + reduce (3*x + 1)
