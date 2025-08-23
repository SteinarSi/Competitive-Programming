main :: IO ()
main = interact (unlines . solve 0 . map read . tail . words)

solve :: Integer -> [Integer] -> [String]
solve _ [] = []
solve 0 (x:xs) = solve x xs
solve m (x:xs) | x `mod` m == 0 = show x : solve 0 xs
               | otherwise      =          solve m xs
