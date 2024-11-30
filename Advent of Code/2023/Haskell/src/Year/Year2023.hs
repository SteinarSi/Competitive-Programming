module Year.Year2023 where

import           Control.Monad (void)

import           Year.Year2023.Day1
import           Year.Year2023.Day10
import           Year.Year2023.Day11
import           Year.Year2023.Day12
import           Year.Year2023.Day13
import           Year.Year2023.Day14
import           Year.Year2023.Day15
import           Year.Year2023.Day16
import           Year.Year2023.Day17
import           Year.Year2023.Day18
import           Year.Year2023.Day19
import           Year.Year2023.Day2
import           Year.Year2023.Day20
import           Year.Year2023.Day21
import           Year.Year2023.Day22
import           Year.Year2023.Day23
import           Year.Year2023.Day24
import           Year.Year2023.Day25
import           Year.Year2023.Day3
import           Year.Year2023.Day4
import           Year.Year2023.Day5
import           Year.Year2023.Day6
import           Year.Year2023.Day7
import           Year.Year2023.Day8
import           Year.Year2023.Day9
import           Meta          (bench, benchAll, solve, test)

test2023 :: IO ()
test2023 = void . benchAll False $ map void [
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
        test Day15,
        test Day16,
        test Day17,
        test Day18,
        test Day19,
        -- test Day20, have to rewrite the test framework before this can work, because the test input is a different problem than the real input
        test Day21,
        test Day22,
        test Day23,
        -- test Day24,
        test Day25
    ]

solve2023 :: IO ()
solve2023 = void . benchAll True $ map void [
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
        solve Day15,
        solve Day16,
        solve Day17,
        solve Day18,
        solve Day19,
        solve Day20,
        solve Day21,
        solve Day22,
        solve Day23,
        -- solve Day24,
        solve Day25
    ]