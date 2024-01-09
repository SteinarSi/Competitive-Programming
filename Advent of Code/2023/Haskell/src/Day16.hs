module Day16(Day16(Day16)) where

import           Control.Monad     (unless)
import           Control.Monad.ST  (ST, runST)
import           Data.Array        (Array, bounds, (!))
import           Data.Array.MArray (MArray (newArray), getElems, readArray,
                                    writeArray)
import           Data.Array.ST     (STArray)
import           Data.Ix           (Ix (..))
import           Meta              (AoC (..))
import           Utils

data Day16 = Day16
instance AoC Day16 Grid Int where
    parse _ = toArray . lines
    part1 _ grid = energize grid ((0,0), East)
    part2 _ = optimizeEnergy
    date _ = 16
    testAnswerPart1 _ = 46
    testAnswerPart2 _ = 51

type Pos = (Int, Int)
type Grid = Array Pos Char

data Dir = North | South | West | East
    deriving (Eq, Ord, Ix)

optimizeEnergy :: Grid -> Int
optimizeEnergy grid = maximum $ map (energize grid) $ concatMap concat [
        [ [((x,0), South), ((x,my), North)] | x <- range (0, mx) ],
        [ [((0,y), East ), ((mx,y), West )] | y <- range (0, my) ]
    ]
    where (mx, my) = snd (bounds grid)

energize :: Grid -> (Pos,Dir) -> Int
energize grid (start, sdir) = runST $ do
    let (b, e) = bounds grid
    eng <- newArray (bounds grid) False
    memo <- newArray ((b, North), (e, East)) False
    energize' start start sdir eng memo
    countEnergy eng

    where
        energize' :: Pos -> Pos -> Dir -> STArray s Pos Bool -> STArray s (Pos, Dir) Bool -> ST s ()
        energize' ori pos dir eng memo | not (inRange (bounds grid) pos) = pure ()
                                       | otherwise = do
            energized <- readArray memo (pos, dir)
            unless energized $ do
                    writeArray eng pos True
                    writeArray memo (pos, dir) True
                    case (grid ! pos, dir) of
                        ('.' , North) -> goNorth
                        ('.' , South) -> goSouth
                        ('.' , East ) -> goEast
                        ('.' , West ) -> goWest
                        ('-' , East ) -> goEast
                        ('-' , West ) -> goWest
                        ('-' , _    ) -> goEast >> goWest
                        ('|' , North) -> goNorth
                        ('|' , South) -> goSouth
                        ('|' , _    ) -> goNorth >> goSouth
                        ('/' , North) -> goEast
                        ('/' , South) -> goWest
                        ('/' , West ) -> goSouth
                        ('/' , East ) -> goNorth
                        ('\\', North) -> goWest
                        ('\\', South) -> goEast
                        ('\\', East ) -> goSouth
                        ('\\', West ) -> goNorth
                        _             -> error "bruh"
            where
                goNorth = energize' ori (pos +++ (0,-1)) North eng memo
                goSouth = energize' ori (pos +++ (0, 1)) South eng memo
                goWest  = energize' ori (pos +++ (-1,0)) West  eng memo
                goEast  = energize' ori (pos +++ ( 1,0)) East  eng memo

        countEnergy :: STArray s Pos Bool -> ST s Int
        countEnergy = ((length . filter id) <$>) . getElems
