main :: IO ()
main = do
    [n, m] <- fmap (map read . words) getLine
    putStrLn $ case n-m of
        1             -> "Dr. Chaz needs 1 more piece of chicken!"
        -1            -> "Dr. Chaz will have 1 piece of chicken left over!"
        x | x < 0     -> "Dr. Chaz will have " ++ show (m-n) ++ " pieces of chicken left over!"
          | otherwise -> "Dr. Chaz needs " ++ show (n-m) ++ " more pieces of chicken!"
