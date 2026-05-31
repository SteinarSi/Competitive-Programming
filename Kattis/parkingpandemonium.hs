main :: IO ()
main = do
    [m,_,c] <- fmap (map read . words) getContents
    print (m*c)
