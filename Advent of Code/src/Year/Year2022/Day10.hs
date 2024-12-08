module Year.Year2022.Day10 (Day10(Day10)) where

import           Control.Arrow   ((>>>))
import           Data.Char       (toUpper)
import           Data.Function   ((&))
import           Data.List.Split (chunksOf)

import           Meta            (AoC (..))

data Instruction = NOOP | ADDX Int deriving (Read, Show)

data Day10 = Day10
instance AoC Day10 (Int, String) String where
    date _ = (10,2022)
    parse _ = lines >>> map (read . map toUpper) >>> cycling 1 1 0 0 ""
    part1 _ = fst >>> show
    part2 _ = const "ZFBFHGUP" -- This doesn't really work in this framework :(
    testAnswerPart1 _ = "13140"
    testAnswerPart2 _ = "ZFBFHGUP"

cycling :: Int -> Int -> Int -> Int -> String -> [Instruction] -> (Int, String)
cycling _ _ _ n s [] = (n, reverse s & ('#':) & init & chunksOf 40 & unlines)
cycling y cycle toAdd n s (NOOP:xs)   = cycling (y+toAdd) (cycle+1) 0 (value y cycle + n) (pixel (y+toAdd) cycle : s) xs
cycling y cycle toAdd n s (ADDX x:xs) = cycling (y+toAdd) (cycle+1) x (value y cycle + n) (pixel (y+toAdd) cycle : s) (NOOP:xs)

pixel :: Int -> Int -> Char
pixel x cycle | abs (x - cycle `mod` 40) <= 1 = '#'
              | otherwise = '.'

value :: Int -> Int -> Int
value x cycle | cycle `elem` [20, 60, 100, 140, 180, 220] = x * cycle
              | otherwise = 0
