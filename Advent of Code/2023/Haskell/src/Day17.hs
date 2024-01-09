module Day17(Day17(Day17)) where

import           Control.Applicative (liftA2)
import           Control.Monad       (filterM)
import           Control.Monad.ST    (ST, runST)
import           Data.Array          (Array, bounds, (!))
import           Data.Array.MArray   (MArray (newArray), readArray, writeArray)
import           Data.Array.ST       (STUArray)
import           Data.Char           (digitToInt)
import           Data.Ix             (Ix (range))
import           Data.Sequence       (Seq (..), fromList, (><))
import           Meta                (AoC (..))
import           Utils               (inBounds, toArray)

data Day17 = Day17
instance AoC Day17 Grid Int where
    parse _ = toArray . map (map digitToInt) . lines
    part1 _ = crucible (1,3)
    part2 _ = crucible (4,10)
    date _ = 17
    testAnswerPart1 _ = 102
    testAnswerPart2 _ = 94

type Grid = Array (Int, Int) Int
data LatLon = Lon | Lat
    deriving (Eq, Ord, Ix, Show, Bounded)

crucible :: (Int,Int) -> Grid -> Int
crucible (mindist, maxdist) grid = runST $ do
    dist <- newArray ((0,0,Lon), (mx,my,Lat)) 999999999999999999
    writeArray dist (0,0,Lon) 0
    writeArray dist (0,0,Lat) 0
    inQ <- newArray ((0,0,Lon), (mx,my,Lat)) False
    writeArray inQ (0,0,Lon) True
    writeArray inQ (0,0,Lat) True
    runCrucible inQ dist (fromList [(0,0,Lat), (0,0,Lon)])

    where
        (mx,my) = snd (bounds grid)

        runCrucible :: STUArray s (Int,Int,LatLon) Bool -> STUArray s (Int,Int,LatLon) Int -> Seq (Int,Int,LatLon) -> ST s Int
        runCrucible _ dist Empty = liftA2 min (readArray dist (mx,my,Lat)) (readArray dist (mx,my,Lon))
        runCrucible inQ dist (u :<| queue) = do
            distu <- readArray dist u
            next <- filterM (\v -> do
                distv <- readArray dist v
                let costv = distu + actDist u v
                if costv < distv
                    then do
                        writeArray dist v costv
                        not <$> readArray inQ v
                    else pure False
                ) (neighbours u)

            runCrucible inQ dist (queue >< fromList next)

        actDist :: (Int, Int, LatLon) -> (Int, Int, LatLon) -> Int
        actDist (ux,uy,_) (vx,vy,_) = sum ([ grid ! p | p <- range ((ux,uy), (vx,vy)) ++ range ((vx,vy), (ux,uy)) ]) - grid ! (ux,uy)

        neighbours :: (Int, Int, LatLon) -> [(Int,Int,LatLon)]
        neighbours (x,y,Lat) = [ (x+dx,y,Lon) | dx <- deltas, inBounds grid (x+dx,y) ]
        neighbours (x,y,Lon) = [ (x,y+dy,Lat) | dy <- deltas, inBounds grid (x,y+dy) ]

        deltas :: [Int]
        deltas = [-maxdist.. -mindist] ++ [mindist..maxdist]
