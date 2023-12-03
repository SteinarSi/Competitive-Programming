module Main where

import Meta (solve, test)
import Utils (benchmark)
import Day1
import Day2
import Day3

main :: IO ()
main = benchmark testAll

doAll :: IO ()
doAll = testAll >> solveAll
    
testAll :: IO ()
testAll = do
    test Day1
    test Day2
    test Day3
    pure ()

solveAll :: IO ()
solveAll = do
    solve Day1
    solve Day2
    solve Day3
