module Year.Year2022.Day4 (Day4(Day4)) where

import           Control.Arrow   ((>>>))
import           Data.List.Split (splitOn)

import           Meta            (AoC (..))

data Day4 = Day4
instance AoC Day4 [((Int,Int), (Int,Int))] Int where
    parse _ = lines >>> map (splitOn "," >>> map (splitOn "-" >>> map read) >>> (\[[a,b],[c,d]] -> ((a,b),(c,d))))
    part1 _ = filter fullOverlap >>> length
    part2 _ = filter overlap >>> length
    date _  = 4
    year _  = 2022
    testAnswerPart1 _ = 2
    testAnswerPart2 _ = 4

overlap :: ((Int,Int), (Int,Int)) -> Bool
overlap ((a,b),(c,d)) = or [
        c <= a && a <= d,
        c <= b && b <= d,
        a <= c && c <= b,
        a <= d && d <= b
    ]

fullOverlap :: ((Int,Int), (Int,Int)) -> Bool
fullOverlap ((a,b),(c,d)) = c <= a && b <= d || a <= c && d <= b
