main :: IO ()
main = do
    n <- read <$> getLine
    print (solve n)

solve :: Int -> Int
solve n = length (takeWhile (<=n) pyramids)

pyramids :: [Int]
pyramids = map (\i -> (i*(i+1)) `div` 2) [1..]
