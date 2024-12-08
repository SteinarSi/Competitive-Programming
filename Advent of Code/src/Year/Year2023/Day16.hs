module Year.Year2023.Day16 (Day16(Day16)) where

import           Control.Arrow      ((>>>))
import           Control.Monad      (unless)
import           Control.Monad.ST   (ST, runST)
import           Data.Array.Unboxed (UArray, bounds, (!))
import           Data.Array.MArray  (newArray, getElems, readArray, writeArray)
import           Data.Array.ST      (STUArray)
import           Data.Ix            (Ix(..))

import           Meta               (AoC (..))
import           Utility.Misc       (inBounds, toArray, (+++))

data Day16 = Day16
instance AoC Day16 Grid Int where
    date _ = (16,2023)
    parse _ = lines >>> toArray
    part1 _ grid = energize grid ((0,0), East)
    part2 _ = optimizeEnergy
    testAnswerPart1 _ = 46
    testAnswerPart2 _ = 51

type Pos = (Int, Int)
type Grid = UArray Pos Char

data Dir = North | South | West | East
    deriving (Eq, Ord, Ix)

optimizeEnergy :: Grid -> Int
optimizeEnergy grid = maximum $ map (energize grid) $ concatMap concat [
        [ [((0,x), South), ((my,x), North)] | x <- range (0, mx) ],
        [ [((y,0), East ), ((y,mx), West )] | y <- range (0, my) ]
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
        energize' :: Pos -> Pos -> Dir -> STUArray s Pos Bool -> STUArray s (Pos, Dir) Bool -> ST s ()
        energize' ori pos dir eng memo | not (inBounds grid pos) = pure ()
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
                goNorth = energize' ori (pos +++ (-1, 0)) North eng memo
                goSouth = energize' ori (pos +++ ( 1, 0)) South eng memo
                goWest  = energize' ori (pos +++ ( 0,-1)) West  eng memo
                goEast  = energize' ori (pos +++ ( 0, 1)) East  eng memo

        countEnergy :: STUArray s Pos Bool -> ST s Int
        countEnergy = getElems >>> ((filter id >>> length) <$>)
