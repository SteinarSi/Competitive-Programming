module Main where

import Meta (solve, test)
import Day1
import Day2

main :: IO ()
main = doAll

doAll :: IO ()
doAll = testAll >> solveAll
    
testAll :: IO ()
testAll = do
    test Day1
    test Day2

solveAll :: IO ()
solveAll = do
    solve Day1
    solve Day2
