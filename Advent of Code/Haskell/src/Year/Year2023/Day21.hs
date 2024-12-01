module Year.Year2023.Day21 (Day21(..)) where

import           Control.Applicative
import           Control.Monad       (filterM, forM_, when)
import           Control.Monad.ST    (ST, runST)
import           Data.Array          (Array, Ix (range), bounds)
import           Data.Array.MArray   (MArray (getBounds, newArray), readArray,
                                      writeArray)
import           Data.Array.ST       (STArray)
import           Data.Ix             (Ix (inRange))
import           Data.Sequence       (Seq (..), (><))
import qualified Data.Sequence       as Seq
import           Meta                (AoC (..))
import           Utility.Misc

data Day21 = Day21
instance AoC Day21 (Array Pos Char, Pos) Integer where
    parse _ s = (arr, start)
        where arr = toArray (lines s)
              start = head [ p | p <- range (bounds arr), arr !!! p == 'S' ]
    part1 _ = infiniteStepCount 0 64

     -- TODO why divide here???
    part2 _ = leastSquares (fromIntegral (goal `div` 131)) . squares
    -- part2 _ _ = g goal
    -- part2 _ _ = f goal
    -- part2 _ = infiniteStepCount 1000
    date _ = 21
    year _ = 2023
    testAnswerPart1 _ = 42
    testAnswerPart2 _ = -1

type Pos = (Int,Int)

goal :: Int
goal = 26501365

leastSquares :: Integer -> (Integer, Integer, Integer) -> Integer
leastSquares x (y0, y1, y2) = trace' (b0, b1, b2) $ b0 + b1*x + (x*(x-1)`div`2)*(b2-b1)
    where b0 = y0
          b1 = y1 - y0
          b2 = y2 - y1

squares :: (Array Pos Char, Pos) -> (Integer, Integer, Integer)
squares inp@(arr,_) = trace' (height, modd, [modd, modd+height, modd+height*2], [y 0, y 1, y 2]) (y 0, y 1, y 2)
    where
        ((minx,_),(maxx,_)) = bounds arr
        height = fromIntegral (maxx - minx) + 1
        modd = goal `mod` height
        y i = infiniteStepCount i (modd + height*i) inp

infiniteStepCount :: Int -> Int -> (Array Pos Char, Pos) -> Integer
infiniteStepCount gridx limit (arr, start) = runST $ do
    let b = (((-gridx,-gridx), (minx,miny)), ((gridx, gridx), (maxx,maxy)))
        s = ((0,0), start)
    visited <- newArray b False
    writeArray visited s True
    dist <- newArray b 99999999999
    writeArray dist s 0
    bfs visited dist (Seq.singleton s)
    evenSteps visited dist

    where
        ((minx,miny), (maxx, maxy)) = bounds arr

        bfs :: STArray s (Pos,Pos) Bool -> STArray s (Pos,Pos) Int -> Seq (Pos,Pos) -> ST s ()
        bfs _ _ Empty = pure ()
        bfs visited dist (u :<| queue) = do
            rng <- getBounds visited
            distu <- readArray dist u

            when (distu < limit) $ do
                neighs <- filterM (fmap not . readArray visited) (neighbours rng u)

                forM_ neighs $ \v -> writeArray visited v True >> writeArray dist v (distu+1)
                bfs visited dist (queue >< Seq.fromList neighs)

        neighbours :: ((Pos,Pos),(Pos,Pos)) -> (Pos,Pos) -> [(Pos,Pos)]
        neighbours rng (g@(gx,gy),(x,y)) = let normal = map (g,) $ filter (\p -> inBounds arr p && arr !!! p /= '#') [ (x+1,y),(x-1,y),(x,y+1),(x,y-1)  ]
                                               nextgrid = map fst $ filter (\(p,b) -> b && inRange rng p) [
                                                    (((gx-1,gy),(maxx,y)), x == minx),
                                                    (((gx+1,gy),(minx,y)), x == maxx),
                                                    (((gx,gy-1),(x,maxy)), y == miny),
                                                    (((gx,gy+1),(x,miny)), y == maxy)
                                                ]
                                       in  normal ++ nextgrid

        evenSteps :: STArray s (Pos,Pos) Bool -> STArray s (Pos,Pos) Int -> ST s Integer
        evenSteps visited dist = do
            b <- getBounds visited
            candidates <- filterM (\p -> liftA2 (&&) (readArray visited p) (fmap ((even limit == ) . even) (readArray dist p))) (range b)

            -- pure (trace' candidates (length' candidates))
            pure $ length' candidates

-- TODO delete when the other version works
stepCount :: Integer -> (Array Pos Char, Pos) -> Integer
stepCount limit (arr, start) = runST $ do
    let b = bounds arr
    visited <- newArray b False
    writeArray visited start True
    dist <- newArray b 99999999999
    writeArray dist start 0
    bfs visited dist (Seq.singleton start)
    evenSteps visited dist

    where
        bfs :: STArray s Pos Bool -> STArray s Pos Integer -> Seq Pos -> ST s ()
        bfs _ _ Empty = pure ()
        bfs visited dist (u :<| queue) = do
            distu <- readArray dist u
            when (distu < limit) $ do
                neighs <- filterM (fmap not . readArray visited) (neighbours u)

                forM_ neighs $ \v -> writeArray visited v True >> writeArray dist v (distu+1)
                bfs visited dist (queue >< Seq.fromList neighs)

        neighbours :: Pos -> [Pos]
        neighbours (x,y) = filter (\p -> inBounds arr p && arr !!! p /= '#') [ (x+1,y),(x-1,y),(x,y+1),(x,y-1)  ]

        evenSteps :: STArray s Pos Bool -> STArray s Pos Integer -> ST s Integer
        evenSteps visited dist = do
            candidates <- filterM (\p -> liftA2 (&&) (readArray visited p) (even <$> readArray dist p)) (range (bounds arr))

            -- pure (trace' candidates (length candidates))
            pure $ length' candidates

{-
Attempts:

f n = 14304 * n*n + 15596 * n + 3594
f goal = 10046019262846862534
too high

g goal = 13806955004555014394
too high

[3858,33494,93318,181706,301090]




charr: 632257949158206


I fra charr's kode på mitt input:
a0 = 3755
a1 = 33494
a2 = 92811

f goal = 605247138198755
605247138198755
-}
