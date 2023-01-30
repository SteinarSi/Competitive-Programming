import Data.List.Split (splitOn)
import Data.List (intersect)
import Data.Bool (bool)

main :: IO ()
main = do
    inn <- fmap (map (map (map read . splitOn "-") . splitOn ",") . lines) $ readFile "inputs/day4-input.txt"
    print (sum $ map fullOverlap inn, sum $ map overlap inn)

overlap :: [[Int]] -> Int
overlap [[a, b], [c, d]] = bool 1 0 (null $ intersect [a..b] [c..d])

fullOverlap :: [[Int]] -> Int
fullOverlap [[a, b], [c, d]] = bool 0 1 (elem (intersect [a..b] [c..d]) [[a..b], [c..d]])
