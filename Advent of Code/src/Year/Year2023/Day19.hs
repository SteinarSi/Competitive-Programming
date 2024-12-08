module Year.Year2023.Day19 where

import           Data.Bifunctor  (second)
import           Data.Char       (isAlpha)
import           Data.List.Split (splitOn)
import           Data.Map        (Map, fromList, lookup)
import           Meta            (AoC (..))
import           Prelude         hiding (lookup)
import           Utility.Misc


{-
TODO

Del 1 fungerer ikke helt. Jeg prøver å sende hver del som fire 'ranges' på bredde 1, men det blir ikke riktig.
Det ser ut som at intervallene stiger inni rangeCheck, uten at jeg skjønner hvorfor det er riktig for del 2.

Når det funker burde Flow parses og preprosseseres bedre.

-}


data Day19 = Day19
instance AoC Day19 (Flows, [[Range]]) Integer where
    date _ = (19,2023)
    parse _ s = (fromList (map parseFlow flows), map (map (\n -> (n,n)) . extractNaturals) parts)
        where [flows, parts] = splitOn [""] (lines s)
    part1 _ (flows, parts) = let a = map (flowCheck flows "in") parts
                                 b = filter ((>0) . flowCheck flows "in") parts
                                 c = map (sum . map fst) b
                                 d = map (\p -> (p, flowCheck flows "in" p)) parts
                            --  in  trace' parts trace' d trace' b trace' c (sum c)
                             in  sum a
    part2 _ (flows, _) = flowCheck flows "in" (replicate 4 (1,4000))
    testAnswerPart1 _ = 19114
    testAnswerPart2 _ = 167409079868000

parseFlow :: String -> (String, Flow)
parseFlow s = (name, (init rest, last rest))
    where (name,rest) = second (splitOn "," . init . tail) (takeDropWhile isAlpha s)

type Range = (Integer, Integer)
type Flow = ([String], String)
type Flows = Map String Flow

rangeCheck :: [Range] -> String -> ([Range], [[Range]])
rangeCheck rng (p:o:num) = case o of
    '<' -> (r (low, thresh-1), [r (thresh, high)] )
    '>' -> (r (thresh+1, high), [r (low, thresh)])
    '=' | low <= thresh && thresh <= high -> (r (thresh,thresh), [r (low,thresh-1), r (thresh+1,high)])
        | otherwise -> (r (1, 0), [rng])
    c -> bruh c

    where
        r = replace i rng
        i = propIndex p
        thresh = read num
        (low, high) = rng !! i
rangeCheck rng xs = bruh xs

rangeSize :: (Integer, Integer) -> Integer
rangeSize (low, high) | low <= high = 1 + high - low
                      | otherwise   = 0

flowCheck :: Flows -> String -> [Range] -> Integer
flowCheck _ "A" rng = let a = product (map rangeSize rng)
                    --   in  trace' (rng, a) a
                      in  a
flowCheck _ "R" _   = 0
flowCheck flows name rng = case lookup name flows of
    Nothing            -> bruh name
    Just (reqs, other) -> reqCheck reqs other rng

    where
        reqCheck :: [String] -> String -> [Range] -> Integer
        reqCheck [] o rng' = flowCheck flows o rng'
        reqCheck (x:xs) o rng'  = let (check:identifier:_) = splitOn ":" x
                                      (accepted, denied) = rangeCheck rng' check
                                  in  flowCheck flows identifier accepted + sum (map (reqCheck xs o) denied)

propIndex :: Char -> Int
propIndex 'x' = 0
propIndex 'm' = 1
propIndex 'a' = 2
propIndex 's' = 3
propIndex  c  = bruh c
