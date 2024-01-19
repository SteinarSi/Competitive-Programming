main :: IO ()
main = do
    m:_ <- fmap words getLine
    putStrLn $ if read m >= 8
        then "satisfactory"
        else "unsatisfactory"
