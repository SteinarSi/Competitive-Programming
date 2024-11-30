{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module Year.Year2023.Day8 where

import           Data.Char  (isAlphaNum)
import           Data.Map   (Map, fromList, keys, lookup)
import           Data.Maybe (fromJust)
import           Prelude    hiding (lookup)

import           Meta       (AoC (..))

data Day8 = Day8
instance AoC Day8 (String, Graph) Int where
    parse _ s = (dirs, fromList (map parseRow rest))
        where (dirs:_:rest) = lines s
    part1 _ (dirs, graph) = follow graph (cycle dirs) "AAA"
    part2 _ (dirs, graph) = foldr (lcm . follow graph (cycle dirs)) 1 . filter (\s -> last s == 'A')  $ keys graph
    date _ = 8
    year _ = 2023
    testAnswerPart1 _ = 2
    testAnswerPart2 _ = 2

type Graph = Map String (String, String)

parseRow :: String -> (String, (String, String))
parseRow s = (from, (l, r))
    where (from:_:l:r:_) = map (filter isAlphaNum) $ words s

follow :: Graph -> String -> String -> Int
follow graph (d:dirs) u | last u == 'Z' = 0
                        | otherwise     = succ . follow graph dirs . leftRight . fromJust $ lookup u graph
    where leftRight | d == 'L'  = fst
                    | otherwise = snd
