module Year.Year2023.Day6 where

import           Data.List    (find)
import           Data.Maybe   (fromJust)

import           Meta         (AoC (..))
import           Utility.Misc (toTuple)

data Day6 = Day6
instance AoC Day6 [(Int, Int)] Int where
    parse _ = uncurry zip . toTuple . map (map read . tail . words) . lines
    part1 _ = product . map ways
    part2 _ = ways . kerning
    date _ = 6
    year _ = 2023
    testAnswerPart1 _ = 288
    testAnswerPart2 _ = 71503

ways :: (Int, Int) -> Int
ways (time, best) = 1 + victory [time-1, time-2..1] - victory [1..time-1]
    where victory = fromJust . find (\p -> best < (time-p) * p)

kerning :: [(Int, Int)] -> (Int, Int)
kerning xs = (unkern fst, unkern snd)
    where unkern f = read $ concatMap (show . f) xs
