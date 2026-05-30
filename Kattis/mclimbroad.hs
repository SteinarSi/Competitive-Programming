main :: IO ()
main = do
    [w,n] <- fmap (map read . words) getContents
    print (w*5280 `div` n)
