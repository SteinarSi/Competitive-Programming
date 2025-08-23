main :: IO ()
main = do
    [m,n,_] <- fmap (map read . words) getContents
    print (m*n)
