module Year.Year2021.Day2 (Day2(Day2)) where

import           Control.Arrow ((>>>))
import           Data.Char     (toUpper)

import           Meta          (AoC (..))

data Day2 = Day2
instance AoC Day2 [Instruction] Int where
    date _ = (2,2021)
    parse _ = map toUpper >>> lines >>> map read
    part1 _ = navigate (0,0)
    part2 _ = navigateWithAim (0,0) 0
    testAnswerPart1 _ = 150
    testAnswerPart2 _ = 900

data Instruction = DOWN Int | UP Int | FORWARD Int
    deriving Read

navigate :: (Int, Int) -> [Instruction] -> Int
navigate (h, d) [] = h * abs d
navigate (h, d) (c:cs) = case c of
    DOWN x    -> navigate (h, d - x) cs
    UP x      -> navigate (h, d + x) cs
    FORWARD x -> navigate (h + x, d) cs

navigateWithAim :: (Int, Int) -> Int -> [Instruction] -> Int
navigateWithAim (h, d) aim [] = h * d
navigateWithAim (h, d) aim (c:cs) = case c of
    DOWN x    -> navigateWithAim (h, d) (aim + x) cs
    UP x      -> navigateWithAim (h, d) (aim - x) cs
    FORWARD x -> navigateWithAim (h + x, d + x * aim) aim cs
