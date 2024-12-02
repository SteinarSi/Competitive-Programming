module Meta (test, solve, AoC(..), bench, benchAll) where

import           Control.Monad   (forM, void, when, unless)
import           Prelude         hiding (log)

import           Data.Fixed      (showFixed)
import           Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime,
                                  nominalDiffTimeToSeconds)
import           Text.Printf     (printf)

class (Eq answer, Show answer) => AoC day problem answer | day -> problem answer where
    parse :: day -> String  -> problem
    part1 :: day -> problem -> answer
    part2 :: day -> problem -> answer
    date  :: day -> Int
    year  :: day -> Int
    testAnswerPart1 :: day -> answer
    testAnswerPart2 :: day -> answer
    debug :: day -> problem -> [String]
    debug _ _ = []

test :: AoC day problem answer => day -> IO Bool
test day = do
    printf "Testing Year%d/Day%d:\n" (year day) (date day)
    (s1, s2) <- meta day (printf "inputs/year%d/day%d-test.txt" (year day) (date day))
    if s1 /= testAnswerPart1 day
        then putStrLn $ "    Got wrong answer on part 1: " ++ show s1 ++ " /= " ++ show (testAnswerPart1 day)
        else putStrLn   "    Part 1 is correct!"
    if s2 /= testAnswerPart2 day
        then putStrLn $ "    Got wrong answer on part 2: " ++ show s2 ++ " /= " ++ show (testAnswerPart2 day)
        else putStrLn   "    Part 2 is correct!"
    pure (s1 == testAnswerPart1 day && s2 == testAnswerPart2 day)

solve :: AoC day problem answer => day -> IO ()
solve day = do
    printf "Solving Year%d/Day%d:\n" (year day) (date day)
    (s1, s2) <- meta day (printf "inputs/year%d/day%d-input.txt" (year day) (date day))
    putStrLn $ "    Part 1: " ++ show s1
    putStrLn $ "    Part 2: " ++ show s2

meta :: AoC day problem answer => day -> String -> IO (answer, answer)
meta day input = do
    problem <- parse day <$> readFile input
    mapM_ putStrLn (debug day problem)
    pure (part1 day problem, part2 day problem)

bench :: IO a -> IO NominalDiffTime
bench = benchmark True

benchmark :: Bool -> IO a -> IO NominalDiffTime
benchmark log action = do
    start  <- getCurrentTime
    void action
    end    <- getCurrentTime
    let time = diffUTCTime end start
    when log $ putStrLn ("Time spent: " ++ formatTime time)
    pure time

benchAll :: Bool -> [IO a] -> IO NominalDiffTime
benchAll log actions = do
    dash
    total <- fmap sum $ forM actions $ \action -> do
        time <- benchmark log action
        dash
        pure time
    putStrLn ("Time spent in total: " ++ formatTime total)
    pure total

    where dash = putStrLn (replicate 50 '=')

formatTime :: NominalDiffTime -> String
formatTime = printf "%.4fs" . (read :: String -> Double) . showFixed True . nominalDiffTimeToSeconds
