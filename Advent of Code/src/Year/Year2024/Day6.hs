module Year.Year2024.Day6 (Day6(Day6)) where

import           Control.Arrow   ((>>>))
import           Data.Array.Base (UArray, assocs, bounds, indices, (!), (//))
import           Data.Ix         (inRange)
import           Data.Function   ((&))
import           Data.List       (find)
import           Data.Maybe      (fromJust, fromMaybe, isNothing)

import           Meta            (AoC (..))
import           Utility.Misc    (toArray)
import Utility.Structure.HashMap as HM

data Day6 = Day6
instance AoC Day6 (UArray (Int,Int) Char,[(Int,Int)],(Int,Int)) Int where
    parse _ xss = let grid = toArray (lines xss)
                      start = assocs grid
                            & find (snd >>> (=='^'))
                            & fromJust
                            & fst
                      path = patrol grid HM.empty start (-1,0)
                            & fromJust
                  in  (grid,path,start)
    part1 _ (grid,path,start) = length path
    part2 _ (grid,path,start) = blockade grid HM.empty 0 start (-1,0)
    date _  = 6
    year _  = 2024
    testAnswerPart1 _ = 41
    testAnswerPart2 _ = 6

blockade :: UArray (Int,Int) Char -> HM.HashMap [(Int,Int)] -> Int -> (Int,Int) -> (Int,Int) -> Int
blockade grid seen ret c@(y,x) d@(dy,dx) 
        | not (inRange (bounds grid) c) = ret
        | otherwise = blockade grid seen' ret' c' d'
    where
        next  = (y+dy,x+dx)
        willCrash = inRange (bounds grid) next && grid ! next == '#'
        canBlock = not willCrash
                    && inRange (bounds grid) next 
                    && HM.notMember next seen 
                    && isNothing (patrol (grid // [(next,'#')]) seen c d)
        (c',d') | willCrash = (c, (dx,-dy))
                | otherwise = (next,d)
        seen' = HM.insertWith (<>) c [d] seen
        ret' | canBlock  = ret + 1
             | otherwise = ret

patrol :: UArray (Int,Int) Char -> HM.HashMap [(Int,Int)] -> (Int,Int) -> (Int,Int) -> Maybe [(Int,Int)]
patrol grid seen c@(y,x) d@(dy,dx)
        | not (inRange (bounds grid) c) = Just (HM.keys seen)
        | d `elem` fromMaybe [] (HM.lookup c seen) = Nothing
        | otherwise = patrol grid seen' c' d'
    where
        next = (y+dy,x+dx)
        willCrash = inRange (bounds grid) next && grid ! next == '#'
        (c',d') | willCrash = (c, (dx,-dy))
                | otherwise = (next,d)
        seen' = HM.insertWith (<>) c [d] seen
