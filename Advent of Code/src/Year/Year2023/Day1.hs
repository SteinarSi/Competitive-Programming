module Year.Year2023.Day1 where

import           Data.Char (isDigit)
import           Data.List (find, isPrefixOf)

import           Meta      (AoC (..))

data Day1 = Day1
instance AoC Day1 [[(Bool, Int)]] Int where
    date _ = (1,2023)
    parse _ = map parseRow . lines
    part1 _ = sum . map ((\s -> 10 * head s + last s) . map snd . filter fst)
    part2 _ = sum . map ((\s -> 10 * head s + last s) . map snd)
    testAnswerPart1 _ = 297 -- had to change this, since we have two test inputs
    testAnswerPart2 _ = 281

parseRow :: String -> [(Bool, Int)]
parseRow "" = []
parseRow (x:xs) | isDigit x = (True, read [x]) : parseRow xs
parseRow xs = maybe [] (pure . (False,) . snd) (find ((`isPrefixOf` xs) . fst) numbers) ++ parseRow (tail xs)

numbers :: [(String, Int)]
numbers = [
        ("zero", 0),
        ("one", 1),
        ("two", 2),
        ("three", 3),
        ("four", 4),
        ("five", 5),
        ("six", 6),
        ("seven", 7),
        ("eight", 8),
        ("nine", 9)
    ]
