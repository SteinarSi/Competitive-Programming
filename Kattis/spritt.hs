main :: IO ()
main = do
    _:s:xs <- fmap (map read . words) getContents
    putStrLn (if s >= sum xs then "Jebb" else "Neibb")