import Control.Monad (when)
import Data.List (genericLength)

main :: IO ()
main = getLine >>= flip when (getLine >>= print . combs . map read . words >> main) . (/="0")

combs :: [Integer] -> Integer
combs [] = 1
combs (x:xs) = combs smol * combs bigg * choose (genericLength xs) (genericLength smol)
    where smol = filter (< x) xs
          bigg = filter (>=x) xs

choose :: Integer -> Integer -> Integer
choose n 0 = 1
choose n k | 2 * k > n = choose n (n-k)
           | otherwise = div (n * choose (n-1) (k-1)) k
