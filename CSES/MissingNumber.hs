main :: IO ()
main = fmap read getLine >>= \n -> getLine >>= \ns -> print (n*(n+1) `div` 2 - sum (map read $ words ns))