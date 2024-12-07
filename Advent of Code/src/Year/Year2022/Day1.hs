module Year.Year2022.Day1 (Day1(Day1)) where

import           Control.Arrow   ((>>>))
import           Data.List.Split (splitWhen)

import           Meta            (AoC (..))
import           Utility.Misc    (read', revSort)

data Day1 = Day1
instance AoC Day1 [Int] Int where
    parse _ = lines >>> splitWhen null >>> map (map read' >>> sum) >>> revSort
    part1 _ = head
    part2 _ = take 3 >>> sum
    date _  = 1
    year _  = 2022
    testAnswerPart1 _ = 24000
    testAnswerPart2 _ = 45000