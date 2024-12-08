module Year.Year2024.Day8 (Day8(Day8)) where

import           Control.Arrow             ((>>>), (***), (&&&))
import           Data.Array.Base           (UArray, assocs)
import           Data.Char                 (ord)
import           Data.Function             ((&))
import qualified Data.IntMap               as M
import           Data.Tuple                (swap)

import           Meta                      (AoC (..))
import           Utility.Misc              (inBounds, toArray, (+++))
import qualified Utility.Structure.HashSet as HS

data Day8 = Day8
instance AoC Day8 (UArray (Int,Int) Char, [[(Int,Int)]]) Int where
    parse _ = lines >>> toArray >>> id &&& antennas
    part1 _ (grid,xss) = map (antinodes (segment grid)) xss
                    & HS.unions
                    & HS.size
    part2 _ (grid,xss) = map (\xs -> HS.fromList xs <> antinodes (line grid) xs) xss
                    & HS.unions
                    & HS.size
    date _  = 8
    year _  = 2024
    testAnswerPart1 _ = 14
    testAnswerPart2 _ = 34

antinodes :: ((Int,Int) -> (Int,Int) -> [(Int,Int)]) -> [(Int,Int)] -> HS.HashSet
antinodes _ [] = HS.empty
antinodes f (x:xs) = HS.fromList (concatMap (f x) xs) <> antinodes f xs

line :: UArray (Int,Int) Char -> (Int,Int) -> (Int,Int) -> [(Int,Int)]
line grid (ax,ay) (bx,by) = (iterate ((dx , dy)+++) (ax,ay) & takeWhile (inBounds grid))
                         <> (iterate ((-dx,-dy)+++) (ax,ay) & takeWhile (inBounds grid))
    where (dx,dy) = (bx-ax,by-ay)

segment :: UArray (Int,Int) Char -> (Int,Int) -> (Int,Int) -> [(Int,Int)]
segment grid (ax,ay) (bx,by) = filter (inBounds grid) [(ax-dx,ay-dy),(bx+dx,by+dy)]
    where (dx,dy) = (bx-ax,by-ay)

antennas :: UArray (Int,Int) Char -> [[(Int,Int)]]
antennas = assocs
        >>> filter (snd >>> (/='.'))
        >>> map swap
        >>> map (ord *** pure)
        >>> M.fromListWith (<>)
        >>> M.elems
