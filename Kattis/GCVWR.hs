main :: IO ()
main = do
    gcvwr:t:n:items <- fmap (map read . words) getContents
    print $ floor ((gcvwr - t) * 0.9 - sum items)
