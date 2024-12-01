module Year.Year2022 where

import Control.Monad (void)

import Year.Year2022.Day1 
import Year.Year2022.Day2
import Year.Year2022.Day3
import Year.Year2022.Day4
import Year.Year2022.Day5
import Year.Year2022.Day6
import Year.Year2022.Day7
import Year.Year2022.Day8
import Year.Year2022.Day9
import Year.Year2022.Day10
import Year.Year2022.Day11
import Year.Year2022.Day12
import Year.Year2022.Day13
import Year.Year2022.Day14
import Meta

test2022 :: IO ()
test2022 = void . benchAll False $ map void [
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
        test Day14
    ]

solve2022 :: IO ()
solve2022 = void . benchAll True $ map void [
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
        solve Day14
    ]