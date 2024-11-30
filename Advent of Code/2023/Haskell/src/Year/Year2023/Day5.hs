{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Year.Year2023.Day5 where

import           Data.List.Split (chunksOf, splitOn)

import           Meta            (AoC (..))
import           Utility.Misc    (toTriple, toTuple)

data Day5 = Day5
instance AoC Day5 ([Int], [[(Int, Int, Int)]]) Int where
    parse _ inn = (map read (tail (words seeds)), map (map (toTriple . map read . words) . tail) (splitOn [""] rest))
        where (seeds:_:rest) = lines inn
    part1 _ (seeds, maps) = minimum $ foldr (map . convertSingle) seeds (reverse maps)
    part2 _ (seeds, maps) = fst $ minimum $ foldr ((=<<) . convertRange) (map toTuple (chunksOf 2 seeds)) (reverse maps)
    date _ = 5
    year _ = 2023
    testAnswerPart1 _ = 35
    testAnswerPart2 _ = 46

convertSingle :: [(Int, Int, Int)] -> Int -> Int
convertSingle [] x = x
convertSingle ((dest, source, range):xs) x | x >= source && x < source + range = dest + x - source
                                           | otherwise = convertSingle xs x

convertRange :: [(Int, Int, Int)] -> (Int, Int) -> [(Int, Int)]
convertRange [] (x, r) = [(x, r)]
convertRange ((dest, source, range):xs) (x, r)
    | x >= source && x+r <= source + range                          = [(dest + x - source, r)]
    | x <  source && x+r >= source         && x+r < source + range  = (dest, r - source + x) : convertRange xs (x, source - x)
    | x >= source && x < source + range    && x+r >= source + range = (dest + x - source, source + range - x) : convertRange xs (source+range, x+r-source-range)
    | x <  source && x+r >= source + range                          = (source, range) : convertRange xs (x, source - x) ++ convertRange xs (source+range, x+r-source-range)
    | otherwise                                                     = convertRange xs (x, r)
