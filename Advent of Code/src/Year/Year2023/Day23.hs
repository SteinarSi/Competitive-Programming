module Year.Year2023.Day23 (Day23(Day23)) where

import           Control.Arrow             (second, (>>>))
import           Data.Array.Base           (UArray, bounds, indices, (!))
import           Data.Function             ((&))
import           Data.List                 (find)
import           Data.Maybe                (fromJust, mapMaybe)

import           Meta                      (AoC (..))
import           Utility.Misc              (directions, inBounds, supremum, toArray, (+++), trace')
import qualified Utility.Structure.HashMap as HM
import qualified Utility.Structure.HashSet as HS

data Day23 = Day23
instance AoC Day23 (Pos, Pos, HM.HashMap [(Int,Pos,Bool)]) Int where
    date _ = (23,2023)
    parse _ = lines >>> toArray >>> contract
    part1 _ = longLongWaaaaaaalk False
    part2 _ = longLongWaaaaaaalk True
    testAnswerPart1 _ = 94
    testAnswerPart2 _ = 154

type Pos = (Int,Int)

contract :: UArray Pos Char -> (Pos, Pos, HM.HashMap [(Int,Pos,Bool)])
contract grid = (s, t, HM.fromList [ (p, poiNeighbours p) | p <- poi ])
    where
        s = (0,1)
        t = second pred (snd (bounds grid))
        poi = s : t : filter isInteresting (indices grid)

        poiNeighbours :: Pos -> [(Int, Pos, Bool)]
        poiNeighbours p | p == t    = []
                        | otherwise = mapMaybe (reachable False 1 p) (neighs p)

        neighs :: Pos -> [Pos]
        neighs p = filter (\q -> inBounds grid q && grid ! q /= '#') $ map (p+++) directions

        reachable :: Bool -> Int -> Pos -> Pos -> Maybe (Int,Pos,Bool)
        reachable climb cost prev curr 
                | curr == s || curr == t || isInteresting curr = Just (cost,curr,climb)
                | null next = Nothing
                | otherwise = reachable climb' (cost+1) curr next
            where 
                next = neighs curr
                    & find (prev/=)
                    & fromJust
                c = grid ! curr
                climb' = climb || (
                    c == '>' && next == curr +++ (0,-1) ||
                    c == '<' && next == curr +++ (0,1) ||
                    c == '^' && next == curr +++ (1,0) ||
                    c == 'v' && next == curr +++ (-1,0))

        isInteresting :: Pos -> Bool
        isInteresting p = grid ! p /= '#' && length (neighs p) > 2

longLongWaaaaaaalk :: Bool -> (Pos, Pos, HM.HashMap [(Int,Pos,Bool)]) -> Int
longLongWaaaaaaalk canClimb (start, end, graph) = dfs HS.empty start
    where
        dfs :: HS.HashSet -> Pos -> Int
        dfs visited u | u == end  = 0
                      | otherwise = HM.lookup u graph
                            & fromJust
                            & filter (\(_,v,climb) -> (canClimb || not climb) && HS.notMember v visited)
                            & map (\(cost, v, _) -> cost + dfs (HS.insert u visited) v)
                            & supremum
