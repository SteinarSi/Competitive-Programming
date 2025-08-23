main :: IO ()
main = do
    k <- fmap read getLine
    m <- getLine
    f <- getLine
    let l = length . filter id $ zipWith (==) m f
    print (length m - max k l + min k l)
