main :: IO ()
main = do
    (n:h:v:_) <- fmap (map read . words) getLine
    print (4 * max h (n-h) * max v (n-v))