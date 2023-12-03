module Day3 where

import Data.Char (isDigit, isControl)
import Data.Array (Array, (!))
import Data.Bifunctor (bimap)
import Data.List (sort, groupBy)

import Meta (AoC(..))
import Utils (takeDropWhile, padWith, toArray)

data Day3 = Day3
instance AoC Day3 ([((Int, Int), Int)], [Int]) Int where
    parse _ input = search 1 1 (toArray (padWith '.' l)) l
        where l = lines input 
    part1 _ = sum  . snd
    part2 _ = sum . map product . filter ((2==) . length) . map (map snd) . groupBy (\a b -> fst a == fst b) . sort . fst
    date _ = 3
    testAnswerPart1 _ = 4361
    testAnswerPart2 _ = 467835

search :: Int -> Int -> Array (Int, Int) Char -> [String] -> ([((Int, Int), Int)], [Int])
search _ _ _ [] = ([], [])
search _ y arr ("":xss) = search 1 (succ y) arr xss
search x y arr ((s:ss):xss) | isDigit s && any (isSymbol . (arr!)) neighbours = bimap (gears ++) (read dits :) $ search (x+length dits) y arr (rest : xss)
                            | otherwise = search (succ x) y arr (ss:xss)
    where (dits, rest) = takeDropWhile isDigit (s:ss)
          neighbours   = [(h,v) | h <- [x-1..x + length dits], v <- [y-1..y+1]]
          gears        = [ (c, read dits)  | c <- neighbours, arr ! c == '*']

isSymbol :: Char -> Bool
isSymbol c = c /= '.' && not (isDigit c) && not (isControl c)
