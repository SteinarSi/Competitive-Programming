module Year.Year2024.Day1(Day1(Day1)) where

import Control.Arrow ((>>>), (&&&))
import Data.List (sort)

import Meta  (AoC (..))

data Day1 = Day1
instance AoC Day1 ([Int],[Int]) Int where
    date _ = (1,2024)
    parse _ = lines >>> map (words >>> map read) >>> (map head >>> sort) &&& (map last >>> sort)
    part1 _ = uncurry (zipWith (subtract >>> (>>> abs))) >>> sum
    part2 _ = uncurry solulu >>> sum
    testAnswerPart1 _ = 11
    testAnswerPart2 _ = 31

solulu :: [Int] -> [Int] -> [Int]
solulu [] _ = []
solulu xs@(x:_) ys = let (c1,xs') = span (x==) xs
                         (c2,ys') = span (x==) (dropWhile (<x) ys)
                     in  replicate (length c1) (length c2 * x) <> solulu xs' ys'
