main :: IO ()
main = interact (show . negate . minimum . scanl (+) 0 . map read . tail . words) >> putChar '\n'
