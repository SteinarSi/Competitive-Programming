module Day22(Day22(Day22)) where

import           Control.Monad       (filterM, forM, forM_)
import           Control.Monad.ST    (ST, runST)
import           Data.Array          (Array, (!))
import           Data.Array.MArray   (freeze, readArray)
import           Data.Array.ST       (STArray)
import           Data.List           (nub, sort)
import           Data.List.Split     (splitOn)
import           Data.Map            (Map, empty, insertWith, lookup)
import           Data.Maybe          (fromMaybe)
import           Data.Range          (rangesOverlap, (+=+))
import           GHC.Arr             (newSTArray)
import           Meta                (AoC (..))
import           Prelude             hiding (lookup)
import           Utils               (modifyArray)

data Day22 = Day22
instance AoC Day22 ([Int], (Array Int [Int], Array Int [Int])) Int where
    parse _ s = (map idx bricks, dependentMap highest lowest bricks)
        where (highest, lowest, bricks) = falling [] empty empty . sort . zipWith (curry parseBrick) [1..] $ lines s
    part1 _ (bricks, dep) = length $ filter (canRemove dep) bricks
    part2 _ (bricks, (_, dep)) = countDegreeSolve bricks dep
    date _ = 22
    testAnswerPart1 _ = 5
    testAnswerPart2 _ = 7

canRemove :: (Array Int [Int], Array Int [Int]) -> Int -> Bool
canRemove (dependencies, dependents) brick = all (\i -> length (dependencies ! i) > 1) (dependents ! brick)

countDegreeSolve :: [Int] -> Array Int [Int] -> Int
countDegreeSolve bricks dependents = runST $ do
    indegree <- newSTArray (1, length bricks) 0
    forM_ bricks $ \brick -> forM_ (dependents ! brick) $ \i -> modifyArray indegree i succ
    sum <$> forM bricks (impact indegree . pure)

    where
        impact :: STArray s Int Int -> [Int] -> ST s Int
        impact indegree brs | null brs = pure 0
                            | otherwise = do
            next <- fmap (nub . concat) $ forM brs $ \brick -> do
                filterM (\i -> do
                        modifyArray indegree i pred
                        ind <- readArray indegree i
                        pure (ind == 0)
                    ) (dependents ! brick)
            ret <- impact indegree next
            forM_ brs $ \brick -> forM_ (dependents ! brick) (\i -> modifyArray indegree i succ)
            pure (length next + ret)

dependentMap :: Map Int [Brick] -> Map Int [Brick] -> [Brick] -> (Array Int [Int], Array Int [Int])
dependentMap highest lowest bricks = runST $ do
    dependencies <- newSTArray (1, length bricks) []
    dependents   <- newSTArray (1, length bricks) []
    dependentMap' dependencies dependents bricks

    where
        dependentMap' :: STArray s Int [Int] -> STArray s Int [Int] -> [Brick] -> ST s (Array Int [Int], Array Int [Int])
        dependentMap' dependencies dependents [] = liftA2 (,) (freeze dependencies) (freeze dependents)
        dependentMap' dependencies dependents (brick : brs) = do
            let indeg  = findDependencies brick
                outdeg = findDependents   brick
            forM_ indeg  $ \b -> modifyArray dependents   b (idx brick :)
            forM_ outdeg $ \b -> modifyArray dependencies b (idx brick :)

            dependentMap' dependencies dependents brs

        findDependents :: Brick -> [Int]
        findDependents brick = map idx . filter (brick/=) $ filter (collision brick . potential) (fromMaybe [] (lookup (z (to brick) + 1) lowest))

        findDependencies :: Brick -> [Int]
        findDependencies brick = map idx . filter (brick/=) $ filter (collision (potential brick)) (fromMaybe [] (lookup (z (from brick) - 1) highest))

falling :: [Brick] -> Map Int [Brick] -> Map Int [Brick] -> [Brick] -> (Map Int [Brick], Map Int [Brick], [Brick])
falling ans highest lowest [] = (highest, lowest, reverse ans)
falling ans highest lowest (brick : bricks)
    | z (from brick) == 1 || any (collision pot) support = falling (brick:ans) (insertWith (++) (z (to brick)) [brick] highest) (insertWith (++) (z (from brick)) [brick] lowest) bricks
    | otherwise = falling ans highest lowest (pot : bricks)
    where
        pot = potential brick
        support = fromMaybe [] (lookup (z (from pot)) highest)

potential :: Brick -> Brick
potential brick@Brick {from, to} = brick { from  = from { z = z from - 1 }, to = to {z = z to - 1} }

collision :: Brick -> Brick -> Bool
collision b1 b2 = rangesOverlap (rx b1) (rx b2) &&
                  rangesOverlap (ry b1) (ry b2) &&
                  rangesOverlap (rz b1) (rz b2)
    where rx Brick {from, to} = x from +=+ x to
          ry Brick {from, to} = y from +=+ y to
          rz Brick {from, to} = z from +=+ z to

data Pos = Pos {
    x :: Int,
    y :: Int,
    z :: Int
} deriving (Eq)

instance Show Pos where
    show Pos {x, y, z} = show (x, y, z)

data Brick = Brick {
    idx  :: Int,
    from :: Pos,
    to   :: Pos
} deriving (Eq)

instance Show Brick where
    show Brick {idx, from, to} = show idx ++ "=" ++ show from ++ "~" ++ show to

instance Ord Brick where
    (<=) b1 b2 = z (from b1) <= z (from b2)

parseBrick :: (Int, String) -> Brick
parseBrick (idx, s) = Brick {idx, from = Pos fx fy fz, to = Pos tx ty tz}
    where (from:to:_)  = splitOn "~" s
          (fx:fy:fz:_) = map read $ splitOn "," from
          (tx:ty:tz:_) = map read $ splitOn "," to
