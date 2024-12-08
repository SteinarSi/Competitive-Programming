module Year.Year2021.Day1 (Day1(Day1)) where

import           Control.Arrow ((>>>))
import           Data.Function ((&))

import           Meta          (AoC (..))
import           Utility.Misc  (countBy)

data Day1 = Day1
instance AoC Day1 [Int] Int where
    date _ = (1,2021)
    parse _ = lines >>> map read
    part1 _ = increases
    part2 _ xs = zipWith3 (\x y z -> x+y+z) xs (tail xs) (tail (tail xs)) & increases
    testAnswerPart1 _ = 7
    testAnswerPart2 _ = 5

increases :: [Int] -> Int
increases xs = countBy id (zipWith (<) xs (tail xs))
