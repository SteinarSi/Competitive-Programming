main :: IO ()
main = do
    [k,s] <- fmap (map read . words) getLine
    let (q,r) = quotRem s k
    print (q+r)
