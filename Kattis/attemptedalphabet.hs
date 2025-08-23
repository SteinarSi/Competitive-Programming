main :: IO ()
main = do
    xs <- getLine
    let missed = filter (`notElem` xs) ['a'..'z']
    putStrLn $ if null missed
        then "Good job!"
        else missed
