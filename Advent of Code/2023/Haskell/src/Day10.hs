module Day10 where

import           Data.Array (Array, Ix (inRange), bounds, indices)
import           Data.Maybe (mapMaybe)
import           Meta       (AoC (..))

import           Utils      (directions, toArray, (!!!), (+++))

data Day10 = Day10
instance AoC Day10 [(Int, Int)] Int where
    parse _ s = travel arr pos
        where arr = toArray (lines s)
              pos = head [p | p <- indices arr, arr !!! p == 'S']
    part1 _ path = length path `div` 2
    part2 _ = area
    date _ = 10
    testAnswerPart1 _ = 70
    testAnswerPart2 _ = 8

area :: [(Int, Int)] -> Int
area path = (abs (shoelace path) - length path) `div` 2 + 1

shoelace :: [(Int, Int)] -> Int
shoelace xs = sum $ zipWith (\(x1,y1) (x2,y2) -> (y1+y2) * (x1-x2)) xs (tail xs)

travel :: Array (Int, Int) Char -> (Int, Int) -> [(Int, Int)]
travel graph start = head $ mapMaybe (travel' start) (filter (inRange (bounds graph)) $ map (start+++) directions)
    where
        travel' :: (Int, Int) -> (Int, Int) -> Maybe [(Int, Int)]
        travel' prev pos | prev /= start && ds == [start] = Just [pos]
                         | otherwise = case filter (prev/=) $ map (pos+++) ds of
                            [next] -> (prev:) <$> travel' pos next
                            _      -> Nothing
            where
                ds = case graph !!! pos of
                        '|' -> [(0, -1), (0, 1)]
                        '-' -> [(-1, 0), (1, 0)]
                        'L' -> [(0, -1), (1, 0)]
                        'J' -> [(0, -1), (-1, 0)]
                        '7' -> [(0, 1), (-1, 0)]
                        'F' -> [(0, 1), (1, 0)]
                        '.' -> []
                        'S' -> [start]
                        _   -> error "bruh"
