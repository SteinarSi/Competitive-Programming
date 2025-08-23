main :: IO ()
main = interact (show . sum . map read . words)