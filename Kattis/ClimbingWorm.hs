main :: IO ()
main = do
    a:b:h:_ <- fmap (map read . words) getLine
    print $ case h of
        0 -> 0
        _ | a >= h    -> 1
          | otherwise -> ceiling $ 1 + (h-a) / (a-b)
