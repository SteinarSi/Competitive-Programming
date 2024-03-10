main :: IO ()
main = do
    a:b:_ <- fmap (map read . words) getContents
    print (a - b)
