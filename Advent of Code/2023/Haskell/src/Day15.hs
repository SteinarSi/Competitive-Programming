module Day15 where

import           Data.Bifunctor  (bimap)
import           Data.Char       (isControl)
import           Data.List.Split (splitOn)
import           Data.Map        (Map, adjust, assocs, empty, insertWith)
import           Meta            (AoC (..))
import           Utils           (bruh, hashString)

data Day15 = Day15
instance AoC Day15 [String] Int where
    parse _ = splitOn "," . filter (not . isControl)
    part1 _ = sum . map hash
    part2 _ = focusPower . flip configure empty . map reverse
    date _ = 15
    testAnswerPart1 _ = 1320
    testAnswerPart2 _ = 145

type HASHMAP = Map Int [(String, Int)]

insertLens :: String -> Int -> HASHMAP -> HASHMAP
insertLens s l = insertWith (const insert') (hash s) [(s, l)]
    where
          insert' :: [(String, Int)] -> [(String, Int)]
          insert' [] = [(s,l)]
          insert' ((s',l'):xs) | s' == s = (s,l):xs
                               | otherwise = (s',l') : insert' xs

removeLens :: String -> HASHMAP -> HASHMAP
removeLens s = adjust (filter ((/=s) . fst)) (hash s)

configure :: [String] -> HASHMAP -> HASHMAP
configure []              = id
configure (('-':ss):xs)   = configure xs . removeLens (reverse ss)
configure ((x:'=':ss):xs) = configure xs . insertLens (reverse ss) (read [x])
configure xs              = bruh xs

focusPower :: HASHMAP -> Int
focusPower = sum . map (power . bimap succ (zip [1..])) . assocs
    where
        power :: (Int, [(Int, (String,Int))]) -> Int
        power (k, xs) = sum $ map (\(i,(_,l)) -> k * i * l) xs

hash :: String -> Int
hash = hashString 17 256
