main :: IO ()
main = do
    pris <- getLine
    let kostnad = read pris::Double
    plener <- getLine
    let antall = read plener::Double
    plenliste <- getInputs antall []
    print (kostnad * (total plenliste))

total :: [[String]] -> Double
total [] = 0
total (x:xs) = (read (x!!0) :: Double) * (read (x!!1) :: Double) + total xs

getInputs :: (Eq t, Num t) => t -> [[String]] -> IO [[String]]
getInputs antall liste = do
    case antall of
        0 -> pure liste
        _ -> do
            input <- getLine
            let intervall = words input
            getInputs (antall-1) (intervall : liste)