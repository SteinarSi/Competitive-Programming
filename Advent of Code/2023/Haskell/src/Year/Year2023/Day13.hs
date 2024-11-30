module Year.Year2023.Day13 where

import           Data.List.Split (splitOn)
import           Data.Maybe      (fromJust)

import           Data.List       (transpose)
import           Meta            (AoC (..))

data Day13 = Day13
instance AoC Day13 [[String]] Int where
    parse _ = splitOn [""] . lines
    part1 _ = summarize 0
    part2 _ = summarize 1
    date _ = 13
    year _ = 2023
    testAnswerPart1 _ = 405
    testAnswerPart2 _ = 400

summarize :: Int -> [[String]] -> Int
summarize d = sum . map (\xs -> maybe (fromJust (mirror d 1 (transpose xs))) (100*) (mirror d 1 xs))

diff :: Eq a => [[a]] -> [[a]] -> Int
diff = (sum .) . zipWith (((length . filter not) .) . zipWith (==))

mirror :: Int -> Int -> [String] -> Maybe Int
mirror d r xs | r == length xs = Nothing
              | diff (reverse before) after == d = Just r
              | otherwise = mirror d (r+1) xs
    where (before, after) = splitAt r xs
