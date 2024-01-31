main :: IO ()
main = do
    xs <- fmap (map read . tail . words) getContents
    let best = minimum xs
    print $ sum (map (best+) xs) - best*2
