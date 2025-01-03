module Year.Year2023.Day4 where

import           Data.Bifunctor (first)

import           Meta           (AoC (..))
import           Utility.Misc   (mapSome, takeDropWhile)

data Day4 = Day4
instance AoC Day4 [Integer] Integer where
    date _ = (4,2023)
    parse _ = map parseCard . lines
    part1 _ = sum . map score
    part2 _ = duplicateCards . map (1,)
    testAnswerPart1 _ = 13
    testAnswerPart2 _ = 30

score :: Integer -> Integer
score 0 = 0
score x = 2 ^ (x-1)

parseCard :: String -> Integer
parseCard s = fromIntegral $ length (filter (`elem` winning) (tail actual))
    where (winning, actual) = takeDropWhile (/="|") (drop 2 (words s))

duplicateCards :: [(Integer, Integer)] -> Integer
duplicateCards []         = 0
duplicateCards ((c,x):xs) = c + duplicateCards (mapSome (first (+c)) x xs)
