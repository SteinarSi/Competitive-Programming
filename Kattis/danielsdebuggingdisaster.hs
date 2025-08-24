main :: IO ()
main = do
    [k,n,r] <- words <$> getContents
    print (1 - (1 - (1.0-read k) ^ read n) ^ read r)
