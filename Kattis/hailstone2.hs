import           Control.Arrow ((>>>))

main :: IO ()
main = getLine >>= (read >>> hailstone 1 >>> print)

hailstone :: Int -> Int -> Int
hailstone a 1 = a
hailstone a n | even n = hailstone (a+1) (n`div`2)
              | otherwise = hailstone (a+1) (n*3+1)
