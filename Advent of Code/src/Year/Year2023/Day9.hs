module Year.Year2023.Day9 where

import           Meta (AoC (..))

data Day9 = Day9
instance AoC Day9 [[Int]] Int where
    date _ = (9,2023)
    parse _ = map (map read . words) . lines
    part1 _ = predicc
    part2 _ = predicc . map reverse
    testAnswerPart1 _ = 114
    testAnswerPart2 _ = 2

predicc :: [[Int]] -> Int
predicc = sum . map (sum . map last . takeWhile (any (/=0)) . iterate diffs)

diffs :: [Int] -> [Int]
diffs xs = zipWith subtract xs (tail xs)
