module Year.Year2022.Day9 (Day9(Day9)) where

import           Control.Arrow             ((>>>))
import qualified Data.IntSet               as S

import           Meta                      (AoC (..))
import           Utility.Structure.HashSet as HS

data Day9 = Day9
instance AoC Day9 [(Char, Int)] Int where
    parse _ = lines >>> map words >>> map (\[[c],n] -> (c, read n))
    part1 _ = solve 1
    part2 _ = solve 9
    date _  = 9
    year _  = 2022
    testAnswerPart1 _ = 13
    testAnswerPart2 _ = 1

solve :: Int -> [(Char, Int)] -> Int
solve n = simul HS.empty (0,0) (replicate n (0,0))

simul :: HS.HashSet -> (Int, Int) -> [(Int, Int)] -> [(Char, Int)] -> Int
simul seen _    _     []         = HS.size seen
simul seen head chain ((_,0):xs) = simul seen head chain xs
simul seen head chain ((c,n):xs) = simul seen' head' chain' ((c,n-1):xs)
    where chain' = whip head' chain
          head' = move c head
          seen' = HS.insert (last chain') seen

whip :: (Int, Int) -> [(Int, Int)] -> [(Int, Int)]
whip _ [] = []
whip h@(hx,hy) (t@(tx,ty):ts) | abs (tx - hx) > 1 || abs (ty - hy) > 1 = nt : whip nt ts
                              | otherwise = t : whip t ts
    where nt = (tx + signum (hx-tx), ty + signum (hy-ty))

move :: Char -> (Int, Int) -> (Int, Int)
move 'R' (x,y) = (x+1,y)
move 'L' (x,y) = (x-1,y)
move 'U' (x,y) = (x,y-1)
move 'D' (x,y) = (x,y+1)
