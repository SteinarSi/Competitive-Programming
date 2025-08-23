import           Control.Monad (replicateM)

main :: IO ()
main = do
    [u, v, w] <- fmap (map readTuple) (replicateM 3 getLine)
    print (area u v w)

    trees <- fmap (filter (inside u v w) . map readTuple . tail . lines) getContents
    print (length trees)

inside :: (Double, Double) -> (Double, Double) -> (Double, Double) -> (Double, Double) -> Bool
inside u v w p = area u v w == sum [
        area u v p,
        area u w p,
        area v w p
    ]

area :: (Double, Double) -> (Double, Double) -> (Double, Double) -> Double
area (xa, ya) (xb, yb) (xc, yc) = abs (xa * (yb - yc) + xb * (yc-ya) + xc * (ya - yb)) / 2

readTuple :: Read r => String -> (r, r)
readTuple xs = (head w, last w)
    where w = map read $ words xs
