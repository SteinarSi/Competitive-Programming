module Year.Year2023.Day3 (Day3(Day3)) where

import           Control.Arrow   ((>>>), (***))
import           Data.Array.Base (UArray, (!))
import           Data.Char       (isControl, isDigit)
import           Data.Function   ((&))
import           Data.List       (groupBy, sort)

import           Meta            (AoC (..))
import           Utility.Misc    (padWith, takeDropWhile, toArray)

data Day3 = Day3
instance AoC Day3 ([((Int, Int), Int)], [Int]) Int where
    parse _ input = search 1 1 (toArray (padWith '.' l)) l
        where l = lines input
    part1 _ = snd >>> sum
    part2 _ = fst
            >>> sort
            >>> groupBy (\a b -> fst a == fst b)
            >>> map (map snd)
            >>> filter (length >>> (2==))
            >>> map product
            >>> sum
    date _ = 3
    year _ = 2023
    testAnswerPart1 _ = 4361
    testAnswerPart2 _ = 467835

search :: Int -> Int -> UArray (Int, Int) Char -> [String] -> ([((Int, Int), Int)], [Int])
search _ _ _ [] = ([], [])
search _ y arr ("":xss) = search 1 (succ y) arr xss
search x y arr ((s:ss):xss) 
        | isDigit s && any ((arr!) >>> isSymbol) neighbours = search (x+length dits) y arr (rest : xss)
                                                                & (gears ++) *** (read dits :)
        | otherwise                                         = search (succ x) y arr (ss:xss)
    where (dits, rest) = span isDigit (s:ss)
          neighbours   = [(v,h) | h <- [x-1..x + length dits], v <- [y-1..y+1]]
          gears        = [ (c, read dits)  | c <- neighbours, arr ! c == '*']

isSymbol :: Char -> Bool
isSymbol c = c /= '.' && not (isDigit c) && not (isControl c)
