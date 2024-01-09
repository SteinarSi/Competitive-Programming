main :: IO ()
main = do
    input <- fmap (map (map read . words) . tail . lines) getContents
    let goombas = map head input
        heights = map last input
        possible = and $ zipWith (<=) heights $ tail (scanl (+) 0 goombas)
    putStrLn $ if possible
        then "possible"
        else "impossible"
