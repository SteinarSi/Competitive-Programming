module Day23(Day23(Day23)) where

import           Data.Array      (Array)
import           Data.Array.Base (bounds, indices, (!))
import           Data.Bifunctor
import           Data.Map        (Map, fromList, lookup)
import           Data.Maybe      (fromJust, mapMaybe)
import           Data.Set        (Set)
import qualified Data.Set        as S
import           Meta            (AoC (..))
import           Prelude         hiding (lookup)
import           Utils

data Day23 = Day23
instance AoC Day23 (Pos, Pos, Map Pos [(Int,Pos,Bool)]) Int where
    parse _ = contract . toArray . lines
    part1 _ = longLongWaaaaaaalk False
    part2 _ = longLongWaaaaaaalk True
    date _ = 23
    testAnswerPart1 _ = 94
    testAnswerPart2 _ = 154

type Pos = (Int,Int)

contract :: Array Pos Char -> (Pos, Pos, Map Pos [(Int,Pos,Bool)])
contract grid = (s, t, fromList [ (p, poiNeighbours p) | p <- poi ])
    where
        s = (1,0)
        t = first pred (snd (bounds grid))
        poi = s : t : filter isInteresting (indices grid)
        poiNeighbours p = mapMaybe (reachable False 1 p) (neighs p)

        neighs p = filter (\q -> inBounds grid q && grid ! q /= '#') $ map (p+++) directions

        reachable :: Bool -> Int -> Pos -> Pos -> Maybe (Int,Pos,Bool)
        reachable climb cost prev curr | curr == s || curr == t || isInteresting curr = Just (cost,curr,climb)
                                       | null next = Nothing
                                       | otherwise = reachable climb' (cost+1) curr next
            where nexts = filter (prev/=) (neighs curr)
                  [next] = nexts
                  c = grid ! curr
                  climb' = climb || (
                        c == '>' && next == curr +++ (-1,0) ||
                        c == '<' && next == curr +++ (1,0) ||
                        c == '^' && next == curr +++ (0,1) ||
                        c == 'v' && next == curr +++ (0,-1)
                    )

        isInteresting :: Pos -> Bool
        isInteresting p = length (neighs p) > 2

longLongWaaaaaaalk :: Bool -> (Pos, Pos, Map Pos [(Int,Pos,Bool)]) -> Int
longLongWaaaaaaalk canClimb (start, end, graph) = dfs S.empty start
    where
        dfs :: Set Pos -> Pos -> Int
        dfs visited u | u == end  = 0
                      | otherwise = supremum $ map (\(cost, v, _) -> cost + dfs (S.insert u visited) v) next
            where
                next = filter (\(_,v,climb) -> (canClimb || not climb) && S.notMember v visited) (fromJust (lookup u graph))
