main :: IO ()
main = do
    [n, ta, da, tb, db] <- fmap (map read . words) getContents
    putStrLn $ case compare (time n ta da) (time n tb db) of
        GT -> "Bob"
        EQ -> "="
        LT -> "Alice"

time :: Int -> Int -> Int -> Int
time n t d = sum $ replicate n t ++ take n [0,d..]
