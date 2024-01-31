main :: IO ()
main = do
    _:curr:prev:xs <- fmap (map read . words) getContents
    print $ records prev curr xs

records :: Int -> Int -> [Int] -> Int
records _ _ [] = 0
records prev curr (x:xs) | x > prev + curr = 1 + records curr x xs
                         | otherwise = records prev curr xs
