module Year.Year2023.Day7 where

import           Data.List    (elemIndex, group, sort, sortOn)
import           Data.Maybe   (fromJust)

import           Meta         (AoC (..))
import           Utility.Misc (revSort, toTuple)


data Day7 = Day7
instance AoC Day7 [(String, Int)] Int where
    date _ = (7,2023)
    parse _ = map (fmap read . toTuple . words) . lines
    part1 _ = winnings strength
    part2 _ = winnings jokerStrength
    testAnswerPart1 _ = 6440
    testAnswerPart2 _ = 5905

winnings :: Ord a => (String -> a) -> [(String, Int)] -> Int
winnings f = sum . zipWith (\i (_, b) -> i * b) [1..] . sortOn (f . fst)

order :: Bool -> Char -> Int
order True 'J' = -1
order _ x      = fromJust $ elemIndex x "23456789TJQKA"

counts :: String -> [Int]
counts = revSort . map length . group . sort

strength :: String -> [Int]
strength xs = hand (counts xs) : map (order False) xs

jokerStrength :: String -> [Int]
jokerStrength s | null xs   = [6, -1]
                | otherwise = hand (js+y:ys) : map (order True) s
    where xs   = filter (/='J') s
          js   = length s - length xs
          y:ys = counts xs

hand :: [Int] -> Int
hand xs = case xs of
        1:_   -> 0
        2:1:_ -> 1
        2:2:_ -> 2
        3:1:_ -> 3
        3:2:_ -> 4
        4:_   -> 5
        _     -> 6
