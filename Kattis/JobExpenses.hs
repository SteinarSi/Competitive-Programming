main :: IO ()
main = getContents >>= print . negate . sum . filter (<0) . map read . tail . words
