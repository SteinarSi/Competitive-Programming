main :: IO ()
main = do
    samples <- fmap (map ((\[a,b]->(read a,read b / 1000)) . words) . tail . lines) getContents
    print (trapezoid samples)

trapezoid :: [(Double, Double)] -> Double
trapezoid samples = sum $ zipWith (\(x1,y1) (x2,y2) -> (y1+y2) * (x2-x1) / 2) samples (tail samples)
