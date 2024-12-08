module Year.Year2024.DayX (DayX(DayX)) where

-- This module is just a template for new solutions.

import           Meta  (AoC (..))
import           Utility.Misc

data DayX = DayX
instance AoC DayX [String] Int where
    date _ = (x,2024)
    parse _ = lines
    part1 _ = const 0
    part2 _ = const 0
    testAnswerPart1 _ = -1
    testAnswerPart2 _ = -1
    debug _ ps = []
