main :: IO ()
main = do
    input1 <- getLine
    input2 <- getLine
    input3 <- getLine
    let point1 = makePoint input1
    let point2 = makePoint input2
    let point3 = makePoint input3

    if (fst point1) == (fst point2) then putStr (show(fst point3))
    else if (fst point1) == (fst point3) then putStr (show(fst point2))
    else putStr (show(fst point1))

    putStr " "

    if (snd point1) == (snd point2) then putStr (show(snd point3))
    else if (snd point1) == (snd point3) then putStr (show(snd point2))
    else putStr (show(snd point1))


makePoint :: String -> (Int, Int)
makePoint s = ((read ((words s)!!0)::Int), (read ((words s)!!1)::Int))