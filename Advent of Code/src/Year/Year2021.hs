module Year.Year2021 where

import           Year.Year2021.Day1
import           Year.Year2021.Day2

import           Meta (Year(..), solve, solveYear, test, testYear)

data Year2021 = Year2021
instance Year Year2021 where
    year _ = 2021
    tests _ = [
        test Day1,
        test Day2
        ]
    days _ = [
        solve Day1,
        solve Day2
        ]

test2021 :: IO ()
test2021 = testYear Year2021

solve2021 :: IO ()
solve2021 = solveYear Year2021
