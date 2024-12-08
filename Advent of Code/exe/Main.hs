module Main where

import Year.Year2021 (test2021, solve2021)
import Year.Year2022 (test2022, solve2022)
import Year.Year2023 (test2023, solve2023)
import Year.Year2024 (test2024, solve2024)

main :: IO ()
main = do
    test2021
    solve2021
    test2022
    solve2022
    test2023
    solve2023
    test2024
    solve2024
