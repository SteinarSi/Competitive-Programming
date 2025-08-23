main :: IO ()
main = do
    [m, n] <- fmap (map read . words) getLine
    [u,l,r,d] <- fmap (map read . words) getLine
    xs <- fmap lines getContents

    mapM_ (\y -> putStrLn (map (filler y) [1..n+l+r])) [1..u]

    mapM_ (\(y,s) -> putStrLn (concat [
            map (filler y) [1..l],
            s,
            map (filler y) [l+n+1..l+n+r]
        ])) (zip [u+1..u+m] xs)

    mapM_ (\y -> putStrLn (map (filler y) [1..n+l+r])) [u+m+1..u+m+d]

filler :: Int -> Int -> Char
filler x y | odd (x+y) = '.'
           | otherwise = '#'
