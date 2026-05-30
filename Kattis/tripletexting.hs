{-# LANGUAGE MultiWayIf #-}

main :: IO ()
main = do
    xss <- getLine
    let n = length xss `div` 3
        xs = take n xss
        ys = take n (drop n xss)
        zs = drop (2*n) xss
    putStrLn $ if
        | xs == ys  -> xs
        | ys == zs  -> ys
        | otherwise -> zs
