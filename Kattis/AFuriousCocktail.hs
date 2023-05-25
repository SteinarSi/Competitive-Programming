import Data.List (sort)
import Data.Bool (bool)

main :: IO ()
main = interact ((\(n:t:xs) -> bool "NO\n" "YES\n" $ drink (n-1) t (reverse $ sort xs)) . map read . words)

drink :: Int -> Int -> [Int] -> Bool
drink n t [] = True
drink n t (x:xs) = n * t < x && drink (pred n) t xs
