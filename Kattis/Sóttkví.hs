main :: IO ()
main = getContents >>= (\(_:k:d:xs) -> print . length $ filter (\x -> x + 14 <= k+d || x > k+d) xs) . map read . words
