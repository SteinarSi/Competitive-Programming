import           Data.List (sort)

main :: IO ()
main = interact (unwords . map (format . map show) . group [] . sort . map read . tail . words) >> putChar '\n'

format :: [String] -> String
format xs | length xs >= 3 = head xs ++ "-" ++ last xs
          | otherwise      = unwords xs

group :: [Int] -> [Int] -> [[Int]]
group [] [] = []
group xs [] = [reverse xs]
group [] (x:xs) = group [x] xs
group (y:ys) (x:xs) | x == succ y = group (x:y:ys) xs
                    | otherwise   = reverse (y:ys) : group [x] xs
