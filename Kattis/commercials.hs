main :: IO ()
main = do
    (_:p:_) <- fmap (map read . words) getLine
    xs <- fmap (map (subtract p . read) . words) getLine
    print $ loop (maximum xs) 0 xs

loop :: Integer -> Integer -> [Integer] -> Integer
loop best curr []     = max best curr
loop best curr (x:xs) = loop (max best (curr+x)) (max 0 (curr+x)) xs
