import Data.List (sort, sortBy)
import Debug.Trace (trace)
import Data.Ord (comparing, Down (Down))

main :: IO ()
main = interact (show . discount . sortBy (comparing Down) . map read . tail . words)

discount :: [Integer] -> Integer
discount (x:y:z:xs) = x + y + discount xs
discount xs = sum xs
