main :: IO ()
main = getContents >>= print . sum . map pot . tail . words

pot :: String -> Integer
pot xs = read (init xs) ^ read [last xs]
