module Year.Year2024.Day2 (Day2(Day2)) where

import           Control.Arrow ((>>>))
import           Control.Monad (ap)
import           Data.Function ((&))

import           Meta          (AoC (..))

data Day2 = Day2
instance AoC Day2 [[Int]] Int where
    parse _ = lines >>> map (words >>> map read)
    part1 _ = filter safe >>> length
    part2 _ = filter safeish >>> length
    date _  = 2
    year _  = 2024
    testAnswerPart1 _ = 2
    testAnswerPart2 _ = 4

safeish :: [Int] -> Bool
safeish xs = any (\i -> safe (take i xs <> drop (i+1) xs)) [0..length xs - 1] 

safe :: [Int] -> Bool
safe xs = and (zipWith (\x y -> abs (x-y) <= 3) xs (tail xs))
        && (increasing xs || increasing (reverse xs))
    where 
        increasing :: [Int] -> Bool
        increasing = ap (zipWith (<)) tail >>> and
