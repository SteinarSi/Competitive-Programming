main :: IO ()
main = getContents >>= (\(p:q:_) -> print (2022 + p `div` q)) . map read . words