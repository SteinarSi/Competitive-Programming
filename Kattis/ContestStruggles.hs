main :: IO ()
main = do
    [n, k, d, s] <- fmap (map read . words) getContents
    let est = (n*d - k*s) / (n-k)
    if 0 <= est && est <= 100
        then print est
        else putStrLn "impossible"
