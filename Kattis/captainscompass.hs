main :: IO ()
main = do
    [sx,sy] <- map read . words <$> getLine
    [tx,ty] <- map read . words <$> getLine

    putStrLn $ case (compare sy ty, compare sx tx, compare (abs (ty-sy)) (abs (tx-sx))) of
        (EQ, EQ, _)  -> ""
        (LT, EQ, _)  -> "N"
        (GT, EQ, _)  -> "S"
        (EQ, LT, _)  -> "E"
        (EQ, GT, _)  -> "W"
        (LT, LT, LT) -> "NE\nE"
        (LT, LT, GT) -> "NE\nN"
        (LT, LT, EQ) -> "NE"
        (LT, GT, LT) -> "NW\nW"
        (LT, GT, GT) -> "NW\nN"
        (LT, GT, EQ) -> "NW"
        (GT, LT, LT) -> "SE\nE"
        (GT, LT, GT) -> "SE\nS"
        (GT, LT, EQ) -> "SE"
        (GT, GT, LT) -> "SW\nW"
        (GT, GT, GT) -> "SW\nS"
        (GT, GT, EQ) -> "SW"
