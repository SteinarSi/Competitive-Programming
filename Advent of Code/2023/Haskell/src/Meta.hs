{-# LANGUAGE FunctionalDependencies #-}

module Meta (test, solve, AoC(..)) where

import System.Exit (exitFailure)

class AoC day problem | day -> problem where
    parse :: day -> String  -> problem
    part1 :: day -> problem -> Integer
    part2 :: day -> problem -> Integer
    date  :: day -> Integer
    testAnswerPart1 :: day -> Integer
    testAnswerPart2 :: day -> Integer


test :: AoC day problem => day -> IO ()
test day = do
    putStrLn $ "Day " ++ show (date day) ++ ": "
    (s1, s2) <- meta day ("inputs/day" ++ show (date day) ++ "-test.txt")
    if s1 /= testAnswerPart1 day
        then do
            putStrLn $ "    Got wrong answer on part 1: " ++ show s1 ++ " /= " ++ show (testAnswerPart1 day) 
            exitFailure
        else putStrLn "    Part 1 is correct!"
    if s2 /= testAnswerPart2 day
        then do
            putStrLn $ "    Got wrong answer on part 2: " ++ show s2 ++ " /= " ++ show (testAnswerPart2 day)
            exitFailure
        else putStrLn "    Part 2 is correct!"
    putChar '\n'

solve :: AoC day problem => day -> IO ()
solve day = do
    putStrLn $ "Day " ++ show (date day) ++ ": "
    (s1, s2) <- meta day ("inputs/day" ++ show (date day) ++ "-input.txt")
    putStrLn $ "    Part 1: " ++ show s1
    putStrLn $ "    Part 2: " ++ show s2
    putChar '\n'

meta :: AoC day problem => day -> String -> IO (Integer, Integer)
meta day input = do
    problem <- parse day <$> readFile input
    pure (part1 day problem, part2 day problem)


{-
Day Template:

data Day = Day
instance 

-}