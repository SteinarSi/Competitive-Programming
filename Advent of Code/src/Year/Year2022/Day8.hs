module Year.Year2022.Day8 (Day8(Day8)) where

import           Control.Arrow   ((>>>))
import           Data.Array      (Array)
import           Data.Array.Base (UArray, array, bounds, (!))
import           Data.Bool       (bool)
import           Data.Char       (digitToInt)

import           Meta            (AoC (..))

type Dir = Array (Int, Int) [Int]

data Day8 = Day8
instance AoC Day8 (UArray (Int, Int) Int, Dir, Dir, Dir, Dir) Int where
    date _ = (8,2022)
    parse _ = directions . chart . lines
    part1 _ = visible
    part2 _ = nicest
    testAnswerPart1 _ = 21
    testAnswerPart2 _ = 8

nicest :: (UArray (Int, Int) Int, Dir, Dir, Dir, Dir) -> Int
nicest (mapp, high, low, left, right) = maximum (map (\(i,j) -> let a = mapp ! (i,j) in product [
                high  ! (i-1,j) !! a,
                left  ! (i,j-1) !! a,
                low   ! (i+1,j) !! a,
                right ! (i,j+1) !! a
            ]) [(i,j)|i<-[2..m-1], j<-[2..n-1]])
    where (m,n) = snd (bounds mapp)

visible :: (UArray (Int, Int) Int, Dir, Dir, Dir, Dir) -> Int
visible (mapp, high, low, left, right) = 2*n + (m-2)*2 + length (filter (\(i,j) -> let a = mapp ! (i,j) in (
                high  ! (i-1,j) !! a == i-1 && a > mapp ! (1,j) ||
                left  ! (i,j-1) !! a == j-1 && a > mapp ! (i,1) ||
                low   ! (i+1,j) !! a == m-i && a > mapp ! (m,j) ||
                right ! (i,j+1) !! a == n-j && a > mapp ! (i,n)
            )) [(i,j)|i<-[2..m-1], j<-[2..n-1]])
    where (m,n) = snd (bounds mapp)

directions :: UArray (Int, Int) Int -> (UArray (Int, Int) Int, Dir, Dir, Dir, Dir)
directions mapp = (mapp, high, low, left, right)
    where (m,n) = snd (bounds mapp)
          high  = view (-1, 0) [2..m-1]     [2..n-1]     mapp
          low   = view ( 1, 0) [m-1,m-2..2] [2..n-1]     mapp
          left  = view ( 0,-1) [2..m-1]     [2..n-1]     mapp
          right = view ( 0, 1) [2..m-1]     [n-1,n-2..2] mapp

chart :: [String] -> UArray (Int, Int) Int
chart xs = array ((1, 1), (length xs, length (head xs))) (chartHelper 1 1 xs)
    where chartHelper _ _ [] = []
          chartHelper m n ([]:xss) = chartHelper (m+1) 1 xss
          chartHelper m n ((x:xs):xss) = ((m, n), digitToInt x) : chartHelper m (n+1) (xs:xss)

view :: (Int, Int) -> [Int] -> [Int] -> UArray (Int, Int) Int -> Dir
view (dm, dn) ms ns mapp = ret
    where (m, n) = snd (bounds mapp)
          edges = [((i,j), replicate 10 1) | (i,j) <- ([(i,j) | i <- [1..m], j<-[1,n]]++[(i,j) | i <- [1, m], j<-[1..n]])]
          ret  = array (bounds mapp) (edges ++ [ ((i, j), map (\(t,r) -> bool (r+1) 1 (mapp!(i,j)>=t)) (zip [0..] (ret!(i+dm,j+dn)))) | i<-ms, j<-ns])
