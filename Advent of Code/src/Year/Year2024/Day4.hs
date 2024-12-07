module Year.Year2024.Day4 (Day4(Day4)) where

import           Control.Arrow   ((>>>))
import           Control.Monad   (liftM2)
import           Data.Array.Base (UArray, bounds, indices, listArray, (!))
import           Data.Function   ((&))
import           Data.Ix         (inRange, range)

import           Meta            (AoC (..))
import           Utility.Misc    (toArray)

data Day4 = Day4
instance AoC Day4 (UArray (Int,Int) Char) Int where
    parse _ = lines >>> toArray
    part1 _ = xmas
    part2 _ = mmass
    date _  = 4
    year _  = 2024
    testAnswerPart1 _ = 18
    testAnswerPart2 _ = 9

xmas :: UArray (Int,Int) Char -> Int
xmas grid = indices grid
        & liftM2 extend [(1,0),(1,1),(0,1),(-1,1),(-1,0),(-1,-1),(0,-1),(1,-1)]
        & filter (last >>> inRange (bounds grid))
        & filter (map (grid!) >>> (=="XMAS"))
        & length
    where
        extend :: (Int,Int) -> (Int,Int) -> [(Int,Int)]
        extend (dy,dx) (y,x) = [(y,x),(y+dy,x+dx),(y+2*dy,x+2*dx),(y+3*dy,x+3*dx)]

mmass :: UArray (Int,Int) Char -> Int
mmass grid = range ((1,1),(m-1,n-1))
        & filter (valid grid)
        & length
    where 
        (m,n) = snd (bounds grid)

        valid :: UArray (Int,Int) Char -> (Int,Int) -> Bool
        valid grid ix@(y,x) = grid ! ix == 'A'
                && (ab == ('M','S') || ab == ('S','M'))
                && (cd == ('M','S') || cd == ('S','M'))
            where
                ab = (grid ! (y-1,x-1), grid ! (y+1,x+1))
                cd = (grid ! (y-1,x+1), grid ! (y+1,x-1))
