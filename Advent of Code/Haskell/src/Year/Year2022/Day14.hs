{-# LANGUAGE Strict #-}

module Year.Year2022.Day14 (Day14(Day14)) where

import           Control.Arrow             ((>>>), (&&&))
import           Data.List.Split           (splitWhen)
import           Data.Maybe                (maybe)
import           Meta                      (AoC (..))
import qualified Utility.Structure.HashSet as HS

data Day14 = Day14
instance AoC Day14 (HS.HashSet, Int) Int where
    parse _ = lines >>> concatMap drawLine >>> (HS.fromList &&& (map snd >>> maximum >>> (+2)))
    part1 _ (inn,my) = simulate (my, inn)
    part2 _ (inn,my) = succ $ simulate (my, inn <> HS.fromList (map (,my) [500-my*2..500+my*2]))
    date _  = 14
    year _  = 2022
    testAnswerPart1 _ = 24
    testAnswerPart2 _ = 93

simulate :: (Int, HS.HashSet) -> Int
simulate = simul (500,0) >>> maybe 0 (simulate >>> succ)
    where 
        simul :: (Int, Int) -> (Int, HS.HashSet) -> Maybe (Int, HS.HashSet)
        simul (x,y) c@(my, s) 
            | y >= my                  = Nothing
            | HS.notMember (x,y+1)   s = simul (x,y+1) c
            | HS.notMember (x-1,y+1) s = simul (x-1,y+1) c
            | HS.notMember (x+1,y+1) s = simul (x+1,y+1) c
            | (x,y) == (500, 0)        = Nothing
            | otherwise                = Just ((my, HS.insert (x,y) s))

drawLine :: String -> [(Int, Int)]
drawLine = drawWall . map (\x -> read ('(':x++")")) . filter (/="->") . words
    where drawWall [] = []
          drawWall [x] = []
          drawWall (x:y:xs) = draw x y ++ drawWall (y:xs)
          draw f@(fx, fy) t@(tx, ty) | f == t = [t]
                                     | otherwise = f : draw (fx + signum (tx-fx), fy + signum (ty-fy)) t
