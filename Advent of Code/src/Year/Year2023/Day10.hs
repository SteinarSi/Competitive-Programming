module Year.Year2023.Day10 where

import           Data.Array.Unboxed (UArray, inRange, bounds, indices)
import           Data.Maybe         (mapMaybe)

import           Utility.Misc       (directions, internalPolygonArea, toArray, (!!!), (+++))
import           Meta               (AoC (..))

data Day10 = Day10
instance AoC Day10 [(Int, Int)] Int where
    parse _ s = travel arr pos
        where arr = toArray (lines s)
              pos = head [p | p <- indices arr, arr !!! p == 'S']
    part1 _ path = length path `div` 2
    part2 _ = internalPolygonArea
    date _ = 10
    year _ = 2023
    testAnswerPart1 _ = 70
    testAnswerPart2 _ = 7

travel :: UArray (Int, Int) Char -> (Int, Int) -> [(Int, Int)]
travel graph start = head $ mapMaybe (travel' start) (filter (inRange (bounds graph)) $ map (start+++) directions)
    where
        travel' :: (Int, Int) -> (Int, Int) -> Maybe [(Int, Int)]
        travel' prev pos | prev /= start && ds == [start] = Just [pos]
                         | otherwise = case filter (prev/=) $ map (pos+++) ds of
                            [next] -> (prev:) <$> travel' pos next
                            _      -> Nothing
            where
                ds = case graph !!! pos of
                        '|' -> [(-1, 0), (1, 0)]
                        '-' -> [(0, -1), (0, 1)]
                        'L' -> [(-1, 0), (0, 1)]
                        'J' -> [(-1, 0), (0, -1)]
                        '7' -> [(1, 0), (0, -1)]
                        'F' -> [(1, 0), (0, 1)]
                        '.' -> []
                        'S' -> [start]
                        _   -> error "bruh"
