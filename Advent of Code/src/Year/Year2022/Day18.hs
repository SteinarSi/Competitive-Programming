module Year.Year2022.Day18 (Day18(Day18)) where

import           Control.Arrow        ((>>>), (&&&))
import           Control.Monad        (forM_, filterM, guard)
import           Control.Monad.ST     (runST)
import           Data.Array.ST        (newArray, writeArray, runSTUArray)
import           Data.Array.Unboxed   (UArray, indices, bounds, (!))
import           Data.List.Split      (splitWhen)
import           Data.Function        ((&))
import           Data.Functor         ((<&>))

import           Meta                 (AoC (..))
import           Utility.Misc         (inBounds)
import           Utility.Structure.UF (newUF, find, union)

data Day18 = Day18
instance AoC Day18 (Grid,Int) Int where
    date _ = (18,2022)
    parse _ = lines >>> map (map read . splitWhen (==',')) >>> toGrid >>> id &&& count
    part1 _ = snd
    part2 _ = uncurry airSearch
    testAnswerPart1 _ = 64
    testAnswerPart2 _ = 58

type Grid = UArray (Int, Int, Int) Bool

count :: Grid -> Int
count g = indices g
        & filter (g!)
        & concatMap (friends g)
        & filter ((g!) >>> not)
        & length

toGrid :: [[Int]] -> Grid
toGrid xss = runSTUArray $ do
        ret <- newArray ix False
        forM_ xs (flip (writeArray ret) True)
        pure ret
    where
        xs = map (\[x,y,z] -> (x,y,z)) xss
        ix = bound (head xs, head xs) xs

        bound :: ((Int,Int,Int),(Int,Int,Int)) -> [(Int,Int,Int)] -> ((Int,Int,Int),(Int,Int,Int))
        bound ((!xs,!ys,!zs),(!xl,!yl,!zl)) []            = ((xs-1,ys-1,zs-1),(xl+1,yl+1,zl+1))
        bound ((!xs,!ys,!zs),(!xl,!yl,!zl)) ((x,y,z):xss) = bound ((min x xs, min y ys, min z zs),(max x xl, max y yl, max z zl)) xss

airSearch :: Grid -> Int -> Int
airSearch g res1 = runST $ do
        uf <- newUF (bounds g)
        let air = filter (\i -> not (g ! i)) (indices g)
        sequence_ $ do
            p <- air
            q <- friends g p
            guard (not (g ! q))
            pure (union uf p q)
        corner <- find uf (head air)
        covered <- filterM (find uf >>> (<&> (/=corner))) air
                <&> (map (friends g >>> filter (g!) >>> length) >>> sum)
        pure (res1 - covered)

friends :: Grid -> (Int, Int, Int) -> [(Int, Int, Int)]
friends g (x,y,z) = filter (inBounds g) [(x-1,y,z), (x,y-1,z), (x,y,z-1), (x+1,y,z), (x,y+1,z), (x,y,z+1)]
