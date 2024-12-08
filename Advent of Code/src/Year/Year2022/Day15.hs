module Year.Year2022.Day15 (Day15(Day15)) where

import Data.List (genericLength)
import qualified Data.Set as S

import Control.Arrow ((>>>))
import Control.Concurrent (forkFinally, getNumCapabilities, setNumCapabilities)
import Control.DeepSeq (deepseq)
import Control.Monad (forM_)
import Control.Concurrent.QSemN (QSemN, newQSemN, waitQSemN, signalQSemN)

import           Meta  (AoC (..))
import           Utility.Misc (manhattan)

data Day15 = Day15
instance AoC Day15 [Beacon] Int where
    date _ = (15,2022)
    parse _ = lines >>> map (words >>> parseBeacon)
    part1 _ = const 0
    part2 _ = const 0
    testAnswerPart1 _ = 26
    testAnswerPart2 _ = 56000011
    debug _ ps = []

data Beacon = Beacon {
        beacon :: (Int, Int),
        sensor :: (Int, Int), 
        distance :: Int
    }
    deriving (Show)

-- TODO need to rewrite this to something that works without IO
--  Oh, and it needs to be a lot faster, too
main :: IO ()
main = do
    getNumCapabilities >>= setNumCapabilities
    beacons <- fmap (map (parseBeacon . words) . lines) (readFile "inputs/year2022/day15-test.txt")
    let closeBeacons = filter (\(Beacon (_,sy) _ d) -> abs (sy - target) <= d) beacons
        max' = maximum $ map (\(Beacon (sx,_) _ d) -> sx+d) closeBeacons
        min' = minimum $ map (\(Beacon (sx,_) _ d) -> sx-d) closeBeacons
    print (genericLength $ filter (canHaveBeacon closeBeacons . (,target)) [min'..max'])
    concurrentSearch beacons

concurrentSearch :: [Beacon] -> IO ()
concurrentSearch beacons = do
    qsem <- newQSemN 0
    forM_ beacons (\b -> forkFinally (search b) (const (signalQSemN qsem 1)))
    waitQSemN qsem (length beacons)
    where search beacon = let signals = findSignal beacon beacons
                          in  if null (deepseq signals signals)
                            then pure ()
                            else print (map (\(x, y) -> (x,y, 4000000*x+y)) signals)

target :: Int
target = 2000000

edges :: Beacon -> S.Set (Int, Int)
edges (Beacon (bx,by) ~_ d) = S.fromList $ filter (\(x,y) -> x>=0 && x<=4000000 && y>=0 && y<=4000000) allEdges
    where allEdges = concat [ [(bx+d+1-i, by+i), (bx+i, by-d-1+i), (bx-d-1+i, by-i), (bx-i, by+d+1-i)] | i <- [0..d-1] ]

findSignal :: Beacon -> [Beacon] -> [(Int, Int)]
findSignal source ss = (S.toList $ S.filter (\e -> all (\tc -> manhattan e (beacon tc) > distance tc) toCheck) $ edges source)
    where toCheck = filter (\b -> manhattan (beacon source) (beacon b) <= distance source + distance b) ss

canHaveBeacon :: [Beacon] -> (Int, Int) -> Bool
canHaveBeacon [] _ = False
canHaveBeacon (Beacon s@(sx,sy) b@(bx,by) d :xs) p@(x,y) | b == p = False
                                                         | manhattan s p <= d = True
                                                         | otherwise = canHaveBeacon xs p

parseBeacon :: [String] -> Beacon
parseBeacon (_:_:x:y:_:_:_:_:x':y':_) = Beacon (s1, s2) (b1, b2) (manhattan (s1,s2) (b1,b2))
    where d = read . drop 2 . init
          s1 = d x
          s2 = d y
          b1 = d x'
          b2 = read (drop 2 y')
