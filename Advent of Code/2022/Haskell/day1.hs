import Data.List.Split (splitWhen)
import Data.List (sort)

main :: IO ()
main = fmap (reverse . sort . map (sum . map read) . splitWhen null . lines) (readFile "inputs/day1-input.txt") >>= \(x:y:z:_) -> print (x, x+y+z)
