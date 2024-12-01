module Year.Year2024 where

import Control.Monad (void)

import Year.Year2024.Day1 
import Meta

test2024 :: IO ()
test2024 = void . benchAll False $ map void [
        test Day1
    ]

solve2024 :: IO ()
solve2024 = void . benchAll True $ map void [
        solve Day1
    ]
