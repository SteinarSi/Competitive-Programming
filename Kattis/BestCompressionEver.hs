main :: IO ()
main = do
    n:b:_ <- fmap (map read . words) getContents
    putStrLn $ if n <= 2^(b+1)-1
        then "yes"
        else "no"
