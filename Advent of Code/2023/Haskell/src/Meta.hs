module Meta (test, solve, AoC(..), bench, benchAll) where

import Control.Monad (when, void, forM)
import Data.Time.Clock.System (getSystemTime, SystemTime(systemNanoseconds))
import Data.Word (Word32)
import Prelude hiding (log)

class (Eq answer, Show answer) => AoC day problem answer | day -> problem answer where
    parse :: day -> String  -> problem
    part1 :: day -> problem -> answer
    part2 :: day -> problem -> answer
    date  :: day -> Integer
    testAnswerPart1 :: day -> answer
    testAnswerPart2 :: day -> answer


test :: AoC day problem answer => day -> IO Bool
test day = do
    putStrLn $ "Testing Day " ++ show (date day) ++ ": "
    (s1, s2) <- meta day ("inputs/day" ++ show (date day) ++ "-test.txt")
    if s1 /= testAnswerPart1 day
        then putStrLn $ "    Got wrong answer on part 1: " ++ show s1 ++ " /= " ++ show (testAnswerPart1 day) 
        else putStrLn   "    Part 1 is correct!"
    if s2 /= testAnswerPart2 day
        then putStrLn $ "    Got wrong answer on part 2: " ++ show s2 ++ " /= " ++ show (testAnswerPart2 day)
        else putStrLn   "    Part 2 is correct!"
    pure (s1 == testAnswerPart1 day && s2 == testAnswerPart2 day)

solve :: AoC day problem answer => day -> IO ()
solve day = do
    putStrLn $ "Solving Day " ++ show (date day) ++ ": "
    (s1, s2) <- meta day ("inputs/day" ++ show (date day) ++ "-input.txt")
    putStrLn $ "    Part 1: " ++ show s1
    putStrLn $ "    Part 2: " ++ show s2

meta :: AoC day problem answer => day -> String -> IO (answer, answer)
meta day input = do
    problem <- parse day <$> readFile input
    pure (part1 day problem, part2 day problem)

bench :: IO a -> IO Word32
bench = benchmark True

benchmark :: Bool -> IO a -> IO Word32
benchmark log action = do
    start  <- getSystemTime
    void action
    end    <- getSystemTime
    let time = systemNanoseconds end - systemNanoseconds start
    when log $ putStrLn ("\nTime spent: " ++ show (time `div` 1000000) ++ "ms")
    pure time

benchAll :: Bool -> [IO a] -> IO Word32
benchAll log actions = do
    dash
    total <- fmap sum $ forM actions $ \action -> do
        putChar '\n'
        time <- benchmark log action
        dash
        pure time
    putStrLn ("\nTime spent in total: " ++ show (total `div` 1000000) ++ "ms")
    pure total

    where dash = putStrLn ('\n' : replicate 100 '=')