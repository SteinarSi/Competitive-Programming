main :: IO ()
main = getContents >>= (print . product . map read . lines)
