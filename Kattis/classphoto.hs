main :: IO ()
main = do
    [w,l] <- fmap (map read . words) getContents
    print (w*l)
