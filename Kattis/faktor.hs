main :: IO ()
main = do
    a:i:_ <- fmap (map read . words) getContents
    print (a * (i-1) + 1)
