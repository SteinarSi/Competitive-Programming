main :: IO ()
main = interact (unlines . map (show . convert 0 . foldl (\xs x -> con x : xs) []) . tail  . lines)

convert :: Integer -> [Integer] -> Integer
convert _ [] = 0
convert m (x:xs) | m > x     = convert m         xs - x
                 | otherwise = convert (max m x) xs + x

con :: Char -> Integer
con 'I' = 1
con 'V' = 5
con 'X' = 10
con 'L' = 50
con 'C' = 100
con 'D' = 500
con 'M' = 1000
