main :: IO ()
main = do
    [x, y, x1, y1, x2, y2] <- fmap (map read . words) getLine
    let corners = [(x',y') | x' <- [x1,x2], y' <- [y1,y2]]
        sides = map fst $ filter snd [
                ((x1,y), y `inbetween` (y1,y2)),
                ((x2,y), y `inbetween` (y1,y2)),
                ((x,y1), x `inbetween` (x1,x2)),
                ((x,y2), x `inbetween` (x1,x2))
            ]
        best = minimum $ map (dist (x,y)) (corners ++ sides)
    print best

dist :: (Double,Double) -> (Double,Double) -> Double
dist (x1,y1) (x2,y2) = sqrt (abs (x1-x2)**2 + abs (y1-y2)**2)

inbetween :: Double -> (Double, Double) -> Bool
inbetween x (start, end) = start <= x && x <= end
