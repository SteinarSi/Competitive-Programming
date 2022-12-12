main :: IO ()
main = getLine >> getLine >>= print . count . map read . words

count :: [Int] -> Int
count []  = 0
count [x] = 0
count (x:y:xs) | y < x = x-y + count (x:xs)
               | otherwise = count (y:xs)