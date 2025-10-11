main :: IO ()
main = do
    [w,h] <- fmap (map read . words) getLine
    print (w+h - sqrt (w^2+h^2))
