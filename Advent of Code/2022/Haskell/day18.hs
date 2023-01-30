{-# LANGUAGE TupleSections #-}

import Data.Array (Array, array, (!), (//), range, indices, bounds, inRange)
import Data.List.Split (splitWhen)
import Control.Monad (forM_, forM, unless, filterM)
import Data.Bool (bool)

import Data.UnionFind.IO (fresh, union, equivalent)

type Grid = Array (Int, Int, Int) Bool

main :: IO ()
main = do
    grid <- fmap (toGrid . map (map read . splitWhen (==',')) . lines) (readFile "inputs/day18-input.txt")
    let res1 = count grid
    res2 <- unionSearch grid res1
    print (res1, res2)

count :: Grid -> Int
count g = sum $ map (\p -> bool 0 (sum $ map (bool 1 0 . (g!)) (friends g p)) (g ! p)) (indices g)

toGrid :: [[Int]] -> Grid
toGrid xs = array ix (map (,False) (range ix) ++ map (\[x,y,z] -> ((x,y,z),True)) xs)
    where ix = ((minimum x-1, minimum y-1, minimum z-1), (maximum x+1, maximum y+1, maximum z+1))
          x = map (!!0) xs
          y = map (!!1) xs
          z = map (!!2) xs

unionSearch :: Grid -> Int -> IO Int
unionSearch g res1 = do
    let air = filter (\i -> not (g ! i)) (indices g)
    points <- forM air fresh
    let pointArray = array (bounds g) (zip air points)
    forM_ air $ \p -> forM_ (friends g p) (\i -> unless (g ! i) (union (pointArray ! p) (pointArray ! i)))
    coveredAir <- fmap (sum . map (\p -> length $ filter (g!) (friends g p))) $ filterM (fmap not . equivalent (head points) . (pointArray!)) air
    pure (res1 - coveredAir)

friends :: Grid -> (Int, Int, Int) -> [(Int, Int, Int)]
friends g (x,y,z) = filter (inRange (bounds g)) [(x-1,y,z), (x,y-1,z), (x,y,z-1), (x+1,y,z), (x,y+1,z), (x,y,z+1)]