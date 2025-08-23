main :: IO ()
main = do
    [m, s] <- fmap (foldr (zipWith (+) . map read . words) [0,0] . tail . lines) getContents
    let a = s / (60 * m)
    if a <= 1
        then putStrLn "measurement error"
        else print a
