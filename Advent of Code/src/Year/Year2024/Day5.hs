module Year.Year2024.Day5 (Day5(Day5)) where

import           Control.Arrow   ((>>>), (***), (&&&))
import           Control.Monad   (forM_)
import           Data.Array.Base (UArray, newArray, writeArray, (!))
import           Data.Array.ST   (runSTUArray)
import           Data.Function   ((&))
import           Data.List       (sortBy)
import           Data.List.Split (splitOn)

import           Meta            (AoC (..))
import           Utility.Misc    (read')

data Day5 = Day5
instance AoC Day5 [([Int],[Int])] Int where
    date _ = (5,2024)
    parse _ xss = let (rules,updates) = (lines xss)
                        & span (null >>> not)
                        & parseRules *** ((tail >>> map (splitOn "," >>> map read')))
                  in  map (id &&& sort rules) updates
    part1 _ = filter (uncurry (==))
            >>> map (fst >>> middle)
            >>> sum
    part2 _ = filter (uncurry (/=))
            >>> map (snd >>> middle)
            >>> sum
    testAnswerPart1 _ = 143
    testAnswerPart2 _ = 123

parseRules :: [String] -> UArray (Int,Int) Bool
parseRules xs = runSTUArray $ do
        arr <- newArray ((10,10),(99,99)) False
        forM_ xs (splitOn "|" 
                >>> map read' 
                >>> (head &&& last) 
                >>> flip (writeArray arr) True)
        pure arr

sort :: UArray (Int,Int) Bool -> [Int] -> [Int]
sort rules = sortBy cmp
    where cmp x y | rules ! (x,y) = LT
                  | rules ! (y,x) = GT
                  | otherwise     = EQ

middle :: [Int] -> Int
middle xs = xs !! (length xs `div` 2)
