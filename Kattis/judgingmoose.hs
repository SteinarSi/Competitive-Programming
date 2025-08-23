main :: IO ()
main = do
    left:right:_ <- fmap (map read . words) getLine
    putStrLn $ case (left, right) of
        (0, 0) -> "Not a moose"
        (l, r) | l == r -> "Even " ++ show (l+r)
               | otherwise -> "Odd " ++ show (2 * max l r)
