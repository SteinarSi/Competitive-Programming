{-# LANGUAGE Strict, TupleSections #-}

import Data.List (genericLength)
import qualified Data.Set as S

import Control.Concurrent
import Control.DeepSeq (deepseq)
import Control.Monad (forM_)
import Control.Concurrent.QSemN


import Debug.Trace (trace)

target = 2000000

main :: IO ()
main = do
    getNumCapabilities >>= setNumCapabilities
    beacons <- fmap (map parse . map words . lines) (readFile "day15-input.txt")
    let closeBeacons = filter (\(Beacon (_,sy) _ d) -> abs (sy - target) <= d) beacons
        max' = maximum $ map (\(Beacon (sx,_) _ d) -> sx+d) closeBeacons
        min' = minimum $ map (\(Beacon (sx,_) _ d) -> sx-d) closeBeacons
    print (genericLength $ filter (canHaveBeacon closeBeacons . (,target)) [min'..max'])
    concurrentSearch beacons

concurrentSearch :: [Beacon] -> IO ()
concurrentSearch beacons = do
    qsem <- newQSemN 0
    forM_ beacons search
    waitQSemN qsem (length beacons)
    where search beacon = let signals = findSignal beacon beacons
                          in  if null (signals)
                            then pure ()
                            else print (map (\(x, y) -> (x,y, 4000000*x+y)) signals)

data Beacon = Beacon {
        beacon :: (Int, Int),
        sensor :: (Int, Int), 
        distance :: Int
    }
    deriving (Show)

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

manhattan :: (Int, Int) -> (Int, Int) -> Int
manhattan (a,b) (c,d) = abs (a-c) + abs (b-d)

parse :: [String] -> Beacon
parse (_:_:x:y:_:_:_:_:x':y':_) = Beacon (s1, s2) (b1, b2) (manhattan (s1,s2) (b1,b2))
    where d = read . drop 2 . init
          s1 = d x
          s2 = d y
          b1 = d x'
          b2 = read (drop 2 y')

