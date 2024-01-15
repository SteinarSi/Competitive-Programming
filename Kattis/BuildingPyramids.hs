main :: IO ()
main = getLine >>= print . pyramid 0 0 . read

pyramid :: Integer -> Integer -> Integer -> Integer
pyramid height top n | next > n  = height
                     | otherwise = pyramid (height + 1) next n
    where next = (2 * height + 1) ^ 2 + top
