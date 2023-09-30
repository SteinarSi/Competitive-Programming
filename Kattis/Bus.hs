main :: IO ()
main = interact (unlines . map (show . bus . read) . tail . words)

bus :: Integer -> Integer
bus 1 = 1
bus k = 1 + 2 * bus (k-1)
