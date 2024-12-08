module Year.Year2022.Day13 (Day13(Day13)) where

import           Control.Arrow   ((>>>), (&&&))
import           Data.Bool       (bool)
import           Data.Char       (isDigit)
import           Data.List.Split (splitWhen)

import           Meta            (AoC (..))

data Day13 = Day13
instance AoC Day13 [[Distress]] Int where
    date _ = (13,2022)
    parse _ = lines >>> splitWhen null >>> map (map (insert >>> read))
    part1 _ = zipWith (\i [a,b] -> bool 0 i (a<=b)) [1..] >>> sum
    part2 _ = concat >>> search 2 &&& (search 6 >>> succ) >>> uncurry (*)
    testAnswerPart1 _ = 13
    testAnswerPart2 _ = 140

data Distress = N Int | L [Distress]
    deriving (Read, Show)

instance Eq Distress where
    (==) (N a) (N b) = a == b
    (==) (N a) (L b) = [N a] == b
    (==) (L b) (N a) = [N a] == b
    (==) (L a) (L b) = a == b

instance Ord Distress where
    (<=) (N a) (N b) = a <= b
    (<=) (L a) (L b) = a <= b
    (<=) (L a) (N b) = L    a  <= L [N b]
    (<=) (N a) (L b) = L [N a] <= L    b

search :: Int -> [Distress] -> Int
search t xs = 1 + length (filter (< L [N t]) xs)

insert :: String -> String
insert [] = []
insert ('[':xs) = "L [" ++ insert xs
insert (x:xs) | isDigit x = let (a,b) = span isDigit xs
                            in  "N " ++ [x] ++ a ++ insert b
              | otherwise = x : insert xs
