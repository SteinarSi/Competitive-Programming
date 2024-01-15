main :: IO ()
main = getLine >>= print . area . map read . words

area :: [Double] -> Double
area xs = sqrt (product (map (sp-) xs))
    where sp = sum xs / 2
