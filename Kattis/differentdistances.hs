main :: IO ()
main = do
    xs <- fmap (map read . words) getLine
    case xs of
        [0.0] -> pure ()
        [x1, y1, x2, y2, p] -> do
            print ((abs (x1-x2) ** p + abs (y1-y2) ** p) ** (1/p))
            main
