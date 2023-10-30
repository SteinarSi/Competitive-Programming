main :: IO ()
main = do
    n:_:xs <- fmap (map read . words) getContents
    print (solve n xs)
    
solve :: Integer -> [Integer] -> Int
solve _ [] = 0
solve n (x:xs) | n - x < 0 = length xs + 1
               | otherwise = solve (n-x) xs
