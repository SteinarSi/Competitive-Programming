module Day2 (Day2(..)) where

import           Data.List.Split (splitOn)

import           Meta            (AoC (..))

data Day2 = Day2
instance AoC Day2 [Game] Int where
    parse _ = map parseGame . lines
    part1 _ = sum . map fst . filter possible
    part2 _ = sum . map (product . colors . snd)
    date _ = 2
    testAnswerPart1 _ = 8
    testAnswerPart2 _ = 2286

type Game = (Int, Bag)

data Bag = Bag {
    red   :: !Int,
    green :: !Int,
    blue  :: !Int
}

instance Semigroup Bag where
    (<>) (Bag r1 g1 b1) (Bag r2 g2 b2) = Bag (max r1 r2) (max g1 g2) (max b1 b2)

instance Monoid Bag where
    mempty = Bag 0 0 0

colors :: Bag -> [Int]
colors bag = [red bag, green bag, blue bag]

possible :: Game -> Bool
possible = and . zipWith (>=) [12,13,14] . colors . snd

parseGame :: String -> (Int, Bag)
parseGame s = (read (init idd), foldMap (parseGame' mempty . words) rest)
    where (_:idd:r) = words s
          rest = splitOn ";" $ filter (/= ',') $ unwords r
          parseGame' b (x:"red"  :xs) = parseGame' (b {red = red b + read x}) xs
          parseGame' b (x:"green":xs) = parseGame' (b {green = green b + read x}) xs
          parseGame' b (x:"blue" :xs) = parseGame' (b {blue = blue b + read x}) xs
          parseGame' b _ = b
