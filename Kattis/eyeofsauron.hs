main :: IO ()
main = do
    inn <- getLine
    let b = takeWhile (=='|') inn
    let a = takeWhile (=='|') $ drop (length b + 2) inn
    if a == b 
        then putStrLn "correct"
        else putStrLn "fix"