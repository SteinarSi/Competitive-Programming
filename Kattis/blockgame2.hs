main :: IO ()
main = do
    [n,m] <- fmap (map read . words) getLine
    putStrLn $ if play n m
        then "win"
        else "lose"

play :: Int -> Int -> Bool
play n m
    | n > m = play m n
    | r == 0 = True
    | q <= 1 = not (play (m-n) n)
    | otherwise = True
  where
    (q,r) = quotRem m n
