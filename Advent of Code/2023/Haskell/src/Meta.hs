module Meta (test, solve, AoC(..)) where

class (Eq answer, Show answer) => AoC day problem answer | day -> problem answer where
    parse :: day -> String  -> problem
    part1 :: day -> problem -> answer
    part2 :: day -> problem -> answer
    date  :: day -> Integer
    testAnswerPart1 :: day -> answer
    testAnswerPart2 :: day -> answer


test :: AoC day problem answer => day -> IO Bool
test day = do
    putStrLn $ "\nDay " ++ show (date day) ++ ": "
    (s1, s2) <- meta day ("inputs/day" ++ show (date day) ++ "-test.txt")
    if s1 /= testAnswerPart1 day
        then putStrLn $ "    Got wrong answer on part 1: " ++ show s1 ++ " /= " ++ show (testAnswerPart1 day) 
        else putStrLn "    Part 1 is correct!"
    if s2 /= testAnswerPart2 day
        then putStrLn $ "    Got wrong answer on part 2: " ++ show s2 ++ " /= " ++ show (testAnswerPart2 day)
        else putStrLn "    Part 2 is correct!"
    pure (s1 == testAnswerPart1 day && s2 == testAnswerPart2 day)

solve :: AoC day problem answer => day -> IO ()
solve day = do
    putStrLn $ "\nDay " ++ show (date day) ++ ": "
    (s1, s2) <- meta day ("inputs/day" ++ show (date day) ++ "-input.txt")
    putStrLn $ "    Part 1: " ++ show s1
    putStrLn $ "    Part 2: " ++ show s2

meta :: AoC day problem answer => day -> String -> IO (answer, answer)
meta day input = do
    problem <- parse day <$> readFile input
    pure (part1 day problem, part2 day problem)
