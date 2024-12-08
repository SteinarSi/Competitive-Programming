module Year.Year2023.Day14 where

import           Data.Map     (Map, empty, insertWith, lookup)
import           Prelude      hiding (cycle, lookup)

import           Data.List    (find, transpose)
import           Meta         (AoC (..))
import           Utility.Misc

data Day14 = Day14
instance AoC Day14 [String] Int where
    date _ = (14,2023)
    parse _ = lines
    part1 _ = load . north
    part2 _ = load . simulate empty 0
    testAnswerPart1 _ = 136
    testAnswerPart2 _ = 64

type HashMap = Map Int [([String], Int)]

lookup' :: HashMap -> [String] -> Maybe Int
lookup' hmap xs = do
    xss <- lookup (load xs) hmap
    snd <$> find ((xs==) . fst) xss

insert :: HashMap -> [String] -> Int -> HashMap
insert hmap xs i = insertWith (flip (++)) (load xs) [(xs, i)] hmap

load :: [String] -> Int
load = sum . zipWith (\i x -> i * occurences 'O' x) [1..] . reverse

simulate :: HashMap -> Int -> [String] -> [String]
simulate hmap i xs | i == 1000000000 = xs
                   | otherwise = case lookup' hmap xs of
        Nothing -> simulate (insert hmap xs i) (i+1) (cycle xs)
        Just c  -> sim ((1000000000 - i) `mod` (i - c)) xs
    where
        sim :: Int -> [String] -> [String]
        sim 0 ys = ys
        sim n ys = sim (n-1) (cycle ys)

cycle :: [String] -> [String]
cycle = east . south . west . north

east :: [String] -> [String]
east = map roll

west :: [String] -> [String]
west = map (reverse . roll . reverse)

south :: [String] -> [String]
south = transpose . east . transpose

north :: [String] -> [String]
north = transpose . west . transpose

roll :: String -> String
roll "" = ""
roll [x] = [x]
roll ('.':xs) = '.' : roll xs
roll ('#':xs) = '#' : roll xs
roll ('O':'#':xs) = "O#" ++ roll xs
roll ('O':'.':xs) = '.' : roll ('O':xs)
roll ('O':'O':xs) = case roll ('O':xs) of
    '.':ys -> '.' : roll ('O':ys)
    'O':ys -> 'O' : 'O' : ys
    _      -> error "bruh"
roll _ = error "bruh"
