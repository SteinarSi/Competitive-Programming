module Year.Year2022 where


import           Year.Year2022.Day1 
import           Year.Year2022.Day2
import           Year.Year2022.Day3
import           Year.Year2022.Day4
import           Year.Year2022.Day5
import           Year.Year2022.Day6
import           Year.Year2022.Day7
import           Year.Year2022.Day8
import           Year.Year2022.Day9
import           Year.Year2022.Day10
import           Year.Year2022.Day11
import           Year.Year2022.Day12
import           Year.Year2022.Day13
import           Year.Year2022.Day14
import           Year.Year2022.Day15
import           Year.Year2022.Day18

import           Meta (Year(..), solve, solveYear, test, testYear)

data Year2022 = Year2022
instance Year Year2022 where
    year _ = 2022
    tests _ = [
        test Day1,
        test Day2,
        test Day3,
        test Day4,
        test Day5,
        test Day6,
        test Day7,
        test Day8,
        test Day9,
        test Day10,
        test Day11,
        test Day12,
        test Day13,
        test Day14,
        -- test Day15, -- TODO needs to use the new framework and be muuuuuuuch faster
        test Day18
        ]
    days _ = [
        solve Day1,
        solve Day2,
        solve Day3,
        solve Day4,
        solve Day5,
        solve Day6,
        solve Day7,
        solve Day8,
        solve Day9,
        solve Day10,
        solve Day11,
        solve Day12,
        solve Day13,
        solve Day14,
        -- solve Day15, -- TODO needs to use the new framework and be muuuuuuuch faster
        solve Day18
        ]

test2022 :: IO ()
test2022 = testYear Year2022

solve2022 :: IO ()
solve2022 = solveYear Year2022
