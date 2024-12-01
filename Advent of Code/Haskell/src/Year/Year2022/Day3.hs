module Year.Year2022.Day3 (Day3(Day3)) where

import           Control.Arrow   ((>>>))
import           Data.Char       (ord, isLower)
import           Data.List       (intersect)
import           Data.List.Split (chunksOf)

import           Meta            (AoC (..))
import           Utility.Misc    (tupleToList)

data Day3 = Day3
instance AoC Day3 [String] Int where
    parse _ = lines
    part1 _ = map (\xs -> tupleToList (splitAt (length xs `div` 2) xs)) >>> search
    part2 _ = chunksOf 3 >>> search
    date _  = 3
    year _  = 2022
    testAnswerPart1 _ = 157
    testAnswerPart2 _ = 70

search :: [[String]] -> Int
search = map (foldr1 intersect >>> head >>> priority) >>> sum

priority :: Char -> Int
priority c | isLower c = ord c - 96
           | otherwise = ord c - 38
