module Meta (AoC(..), Year(..), test, solve, bench, benchAll, testYear, solveYear) where

import           Control.Arrow   ((>>>))
import           Control.Monad   (forM, void, when, unless)
import           Prelude         hiding (log)

import           Data.Fixed      (showFixed)
import           Data.Time.Clock (NominalDiffTime, diffUTCTime, getCurrentTime,
                                  nominalDiffTimeToSeconds)
import           Text.Printf     (PrintfArg, printf, formatRealFloat, formatArg)

class (Eq answer, Show answer) => AoC day problem answer | day -> problem answer where
    parse :: day -> String  -> problem
    part1 :: day -> problem -> answer
    part2 :: day -> problem -> answer
    date  :: day -> (Int,Int)
    testAnswerPart1 :: day -> answer
    testAnswerPart2 :: day -> answer
    debug :: day -> problem -> [String]
    debug _ _ = []

class Year year where
    year  :: year -> Int
    tests :: year -> [IO Bool]
    days  :: year -> [IO ()]

test :: AoC day problem answer => day -> IO Bool
test day = do
    let (d,y) = date day
    printf "Testing Year%d/Day%d:\n" y d
    (s1, s2) <- meta day (printf "inputs/test/year%d/day%d.txt" y d)
    if s1 /= testAnswerPart1 day
        then printf "    Got a wrong answer on part 1: %s /= %s\n" (show s1) (show (testAnswerPart1 day))
        else printf "    Part 1 is correct!\n"
    if s2 /= testAnswerPart2 day
        then printf "    Got a wrong answer on part 2: %s /= %s\n" (show s2) (show (testAnswerPart2 day))
        else printf "    Part 2 is correct!\n"
    pure (s1 == testAnswerPart1 day && s2 == testAnswerPart2 day)

solve :: AoC day problem answer => day -> IO ()
solve day = do
    let (d,y) = date day
    printf "Solving Year%d/Day%d:\n" y d
    (s1, s2) <- meta day (printf "inputs/prod/year%d/day%d.txt" y d)
    printf "    Part 1: %s\n" (show s1)
    printf "    Part 2: %s\n" (show s2)

meta :: AoC day problem answer => day -> String -> IO (answer, answer)
meta day input = do
    problem <- parse day <$> readFile input
    mapM_ putStrLn (debug day problem)
    pure (part1 day problem, part2 day problem)

bench :: IO a -> IO (NominalDiffTime, a)
bench = benchmark True

benchmark :: Bool -> IO a -> IO (NominalDiffTime, a)
benchmark log action = do
    start  <- getCurrentTime
    ret    <- action
    end    <- getCurrentTime
    let time = diffUTCTime end start
    when log $ printf "    Time spent: %.4fs\n" time
    pure (time, ret)

benchAll :: Bool -> [IO a] -> IO (NominalDiffTime, [a])
benchAll log actions = do
    dash
    results <- forM actions $ \action -> do
        ret <- benchmark log action
        dash
        pure ret
    let time = sum (map fst results)
        rets = map snd results
    printf "Time spent in total: %.4fs\n" time
    pure (time, rets)

testYear :: Year y => y -> IO ()
testYear y = do
        printf "Testing Year%d:\n" (year y)
        (time,rets) <- benchAll False (tests y)
        let f = length $ filter not rets
            t = length rets
        if f == 0
            then printf "All %d / %d tests from Year%d passed!\n"    t t (year y)
            else printf "Oh no, %d / %d tests from Year%d failed!\n" f t (year y)
        dash

solveYear :: Year y => y -> IO ()
solveYear y = do
        printf "Solving Year%d:\n" (year y)
        void $ benchAll True (days y)
        dash

dash :: IO ()
dash = putStrLn (replicate 50 '=')

instance PrintfArg NominalDiffTime where
    formatArg = nominalDiffTimeToSeconds >>> showFixed True >>> read >>> formatRealFloat
