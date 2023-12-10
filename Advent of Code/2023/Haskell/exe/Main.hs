module Main where

import           Control.Monad (void)

import           Day1
import           Day10
import           Day2
import           Day3
import           Day4
import           Day5
import           Day6
import           Day7
import           Day8
import           Day9
import           Meta          (bench, benchAll, solve, test)

main :: IO ()
main = do
    solveAll
    -- void $ test Day10
    -- void $ bench $ solve Day10


doAll :: IO ()
doAll = testAll >> solveAll

testAll :: IO ()
testAll = void . benchAll False $ map void [
        test Day1,
        test Day2,
        test Day3,
        test Day4,
        test Day5,
        test Day6,
        test Day7,
        test Day8,
        test Day9,
        test Day10
    ]

solveAll :: IO ()
solveAll = void . benchAll True $ map void [
        solve Day1,
        solve Day2,
        solve Day3,
        solve Day4,
        solve Day5,
        solve Day6,
        solve Day7,
        solve Day8,
        solve Day9,
        solve Day10
    ]
