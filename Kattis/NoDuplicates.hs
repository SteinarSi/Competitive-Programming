main = do
    input <- getLine
    let wordss = words input
    if searchDupes wordss then putStrLn "no"
    else putStrLn "yes"

searchDupes [] = False
searchDupes (s:xs) = if elem s xs then True else searchDupes xs