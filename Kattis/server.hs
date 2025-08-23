main :: IO ()
main = do
    n:t:xs <- fmap (map read . words) getContents
    print (length (takeWhile (<=t) (scanl (+) 0 xs)) - 1)
