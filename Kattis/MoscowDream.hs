main :: IO ()
main = do
    a:b:c:n:_ <- fmap (map read . words) getLine
    putStrLn $ if 0 `notElem` [a,b,c] && sum [a,b,c] >= n && n >= 3
        then "YES"
        else "NO"
