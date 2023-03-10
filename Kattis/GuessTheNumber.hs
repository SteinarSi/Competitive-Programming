import System.IO

main = guesser 1 1000

guesser :: Int -> Int -> IO ()
guesser minn maxx = do
    let guess = (minn + maxx) `div` 2
    print guess
    hFlush stdout
    answer <- getLine
    case answer of
        "lower" -> guesser minn (guess-1)
        "higher" -> guesser (guess+1) maxx
        "correct" -> return ()
        _ -> error "bruh"