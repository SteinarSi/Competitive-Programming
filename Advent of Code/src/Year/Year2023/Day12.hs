module Year.Year2023.Day12 where

import           Data.Array      (Array, array, (!))
import           Data.List       (intercalate)
import           Data.List.Split (splitOn)

import           Data.Bifunctor  (Bifunctor (bimap))
import           Meta            (AoC (..))
import           Utility.Misc

data Day12 = Day12
instance AoC Day12 [(String,[Int])] Integer where
    date _ = (12,2023)
    parse _ = map parseRow . lines
    part1 _ = sum . map solulu
    part2 _ = sum . map (solulu . unfold)
    testAnswerPart1 _ = 21
    testAnswerPart2 _ = 525152

solulu :: (String, [Int]) -> Integer
solulu (text, cs) = mem text cs --memo ! (xl, cl)
    where
        xl = length text
        cl = length cs
        memo :: Array (Int, Int) Integer
        memo = array ((0,0), (xl, cl)) [ ((x,c), combs (drop x text) (drop c cs)) | x <- [0..xl], c <- [0..cl] ]

        mem :: [a] -> [b] -> Integer
        mem dx dy = memo ! (xl - length dx, cl - length dy)

        combs :: String -> [Int] -> Integer
        combs [] []           = 1
        combs ('#':_) []      = 0
        combs [] (_:_) = 0
        combs t@('#':_) (c:cs)  | c > length t || '.' `elem` take c t || (length t > c && t !! c == '#') = 0
                                | otherwise = mem (drop (c+1) t) cs
        combs ('.':xs) cs = mem xs cs
        combs ('?':xs) cs = mem xs cs + combs ('#':xs) cs
        combs (_:_) _ = error "bruh"

parseRow :: String -> (String, [Int])
parseRow = fmap (map read . splitOn ",") . toTuple . words

unfold :: (String, [Int]) -> (String, [Int])
unfold = bimap (intercalate "?" . replicate 5) (concat . replicate 5)
