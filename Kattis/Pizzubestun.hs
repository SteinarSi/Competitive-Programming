import           Data.List (sortBy)
import           Data.Ord  (Down (Down), comparing)

main :: IO ()
main = interact (show . zum . sortBy (comparing Down) . map (read . last . words) . tail . lines)

zum :: [Integer] -> Integer
zum []       = 0
zum [x]      = x
zum (x:y:xs) = x + zum xs
