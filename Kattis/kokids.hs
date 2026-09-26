main :: IO ()
main = do
    [_,k,xs] <- words <$> getContents
    print (solve (read k) True (map (=='L') xs))

solve :: Int -> Bool -> [Bool] -> Int
solve k _ [] = k
solve 0 _ _  = 0
solve k g (x:xs)
    | g == x    = solve k (not g) xs
    | otherwise = solve (k-1) g xs
