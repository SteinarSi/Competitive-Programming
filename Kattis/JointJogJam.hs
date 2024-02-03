main :: IO ()
main = do
    [x1,y1,x2,y2,x3,y3,x4,y4] <- fmap (map read . words) getLine
    print $ max (dist (x1,y1) (x2,y2)) (dist (x3,y3) (x4,y4))

dist :: (Double, Double) -> (Double, Double) -> Double
dist (x1,y1) (x2,y2) = sqrt (abs (x1-x2)**2 + abs (y1-y2)**2)
