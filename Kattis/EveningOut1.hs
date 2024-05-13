main :: IO ()
main = do
    [a,b] <- fmap (map read . words) getLine

    let dec = a `mod` b
        inc = b - dec

    print (min inc dec)
