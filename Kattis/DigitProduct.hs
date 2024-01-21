main :: IO ()
main = getLine >>= print . digitProduct . read

digitProduct :: Int -> Int
digitProduct x | x <= 9 = x
               | otherwise = digitProduct (product (filter (>0) (digits x)))

digits :: Int -> [Int]
digits 0 = []
digits x = x `mod` 10 : digits (x `div` 10)
