module Year.Year2023.Day18(Day18(Day18)) where

import           Data.Char    (isAlphaNum)
import           Data.List    (find)
import           Meta         (AoC (..))
import           Utility.Misc

data Day18 = Day18
instance AoC Day18 [[String]] Int where
    parse _ = map words . lines
    part1 _ = digArea . map parseNormalInstruction
    part2 _ = digArea . map parseHexInstruction
    date _ = 18
    year _ = 2023
    testAnswerPart1 _ = 62
    testAnswerPart2 _ = 952408144115

parseNormalInstruction :: [String] -> (D, Int)
parseNormalInstruction (x:y:_) = (read x, read y)
parseNormalInstruction xs      = bruh xs

parseHexInstruction :: [String] -> (D, Int)
parseHexInstruction xs = (read [last num], hex (init num))
    where num = filter isAlphaNum (xs !! 2)

data D = L | R | U | D deriving (Eq, Show)

instance Read D where
    readsPrec _ "" = error "Bruh, empty input"
    readsPrec _ input@(x:_) = case find (elem x . snd) [(L, "L2" :: String), (R, "R0"), (U, "U3"), (D, "D1")] of
        Nothing    -> bruh input
        Just (s,_) -> [(s, "")]

digArea :: [(D,Int)] -> Int
digArea input = border + internalPolygonArea (dig (0,0) input)
    where border = sum (map snd input)

dig :: (Int,Int) -> [(D,Int)] -> [(Int,Int)]
dig (x,y) []         = [(x,y)]
dig (x,y) ((U,c):xs) = (x,y) : dig (x,y+c) xs
dig (x,y) ((D,c):xs) = (x,y) : dig (x,y-c) xs
dig (x,y) ((L,c):xs) = (x,y) : dig (x-c,y) xs
dig (x,y) ((R,c):xs) = (x,y) : dig (x+c,y) xs
