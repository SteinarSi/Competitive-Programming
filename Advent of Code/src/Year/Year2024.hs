module Year.Year2024 where

import           Year.Year2024.Day1
import           Year.Year2024.Day2
import           Year.Year2024.Day3
import           Year.Year2024.Day4
import           Year.Year2024.Day5
import           Year.Year2024.Day6
import           Year.Year2024.Day7
import           Year.Year2024.Day8

import           Meta (Year(..), solve, solveYear, test, testYear)

data Year2024 = Year2024
instance Year Year2024 where
    year _ = 2024
    tests _ = [
        test Day1,
        test Day2,
        test Day3,
        test Day4,
        test Day5,
        test Day6,
        test Day7,
        test Day8
        ]
    days _ = [
        solve Day1,
        solve Day2,
        solve Day3,
        solve Day4,
        solve Day5,
        solve Day6,
        solve Day7,
        solve Day8
        ]

test2024 :: IO ()
test2024 = testYear Year2024

solve2024 :: IO ()
solve2024 = solveYear Year2024
