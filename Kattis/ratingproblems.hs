main :: IO ()
main = interact (unwords . (map (\p (n:k:xs) -> show ((sum xs + (n-k)*p) / n)) [-3, 3] <*>) . pure . map read . words)