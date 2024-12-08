module Year.Year2022.Day2 (Day2(Day2)) where

import           Control.Arrow ((>>>))
import           Data.List     (find)
import           Data.Maybe    (fromJust)

import           Meta          (AoC (..))

data Day2 = Day2
instance AoC Day2 [String] Int where
    date _ = (2,2022)
    parse _ = lines
    part1 _ = map score >>> sum
    part2 _ = map scripted >>> sum
    testAnswerPart1 _ = 15
    testAnswerPart2 _ = 12

scripted :: [Char] -> Int
scripted (a:_:'Y':_) = 3 + s a
scripted (a:_:'Z':_) = 6 + s (fromJust (find (\c -> elem (c, a) w) ("ABC" :: String)))
scripted (a:_:'X':_) =     s (fromJust (find (\c -> elem (a, c) w) ("ABC" :: String)))

score :: [Char] -> Int
score (a:_:b:_) | elem (t b, a) w = s (t b) + 6
                | elem (a, t b) w = s (t b)
                | otherwise       = s (t b) + 3

s :: Char -> Int
s 'A' = 1
s 'B' = 2
s 'C' = 3

t :: Char -> Char
t 'X' = 'A'
t 'Y' = 'B'
t 'Z' = 'C'

w :: [(Char, Char)]
w = [('A', 'C'), ('B', 'A'), ('C', 'B')]
