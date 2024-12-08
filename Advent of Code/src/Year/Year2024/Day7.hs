module Year.Year2024.Day7 (Day7(Day7)) where

import           Control.Arrow   ((>>>), (&&&), second)
import           Data.List       (partition)
import           Data.List.Split (splitOn)

import           Meta            (AoC (..))

data Day7 = Day7
instance AoC Day7 ([(Integer,[Integer])],[(Integer,[Integer])]) Integer where
    date _ = (7,2024)
    parse _ = filter (/=':') 
            >>> lines 
            >>> map (splitOn " " 
            >>> map read 
            >>> (head &&& tail)) 
            >>> partition (possible [(*),(+)])
    part1 _ = fst
            >>> map fst
            >>> sum
    part2 _ = second (filter (possible [(*),(+),(<+>)]))
            >>> uncurry (<>)
            >>> map fst
            >>> sum
    testAnswerPart1 _ = 3749
    testAnswerPart2 _ = 11387

possible :: [(Integer -> Integer -> Integer)] -> (Integer, [Integer]) -> Bool
possible _  (target, []      ) = error "Empty list!"
possible _  (target, [x]     ) = target == x
possible fs (target, (x:y:xs)) = or $ map (\f -> possible fs (target, (x `f` y : xs))) fs

(<+>) :: Integer -> Integer -> Integer
(<+>) a b = a*10^digits + b
    where
        digits = succ (floor (logBase 10 (fromIntegral b)))
