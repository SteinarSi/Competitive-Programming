main :: IO ()
main = interact (unlines . map ((\(i:p:_) -> show i ++ " " ++ show (p*(p+1) `div` 2 + p)) . map read . words) . tail . lines)