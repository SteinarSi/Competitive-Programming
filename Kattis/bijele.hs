main :: IO ()
main = interact (unwords . map show . zipWith (-) [1,1,2,2,2,8] . map read . words) >> putChar '\n'