module Main where

import Control.Monad (void)

import Meta (solve, test, bench, benchAll)
import Day1
import Day2
import Day3
import Day4

main :: IO ()
main = solveAll

doAll :: IO ()
doAll = testAll >> solveAll

testAll :: IO ()
testAll = void . benchAll False $ map void [
        test Day1,
        test Day2,
        test Day3,
        test Day4
    ]

solveAll :: IO ()
solveAll = void . benchAll True $ map void [
        solve Day1,
        solve Day2,
        solve Day3,
        solve Day4
    ]