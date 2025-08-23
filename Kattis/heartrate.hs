
main :: IO ()
main = interact (unlines . map (solve . map read . words) . tail . lines)

solve :: [Double] -> String
solve (b:p:_) = show (pred b * 60 / p) ++ " " ++ show (b * 60 / p) ++ " " ++ show (succ b * 60 / p)

