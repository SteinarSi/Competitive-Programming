main :: IO()
main = do
    inp <- getLine
    let antall = read inp::Int
    inputs <- takeInput antall []
    handleInputs antall (reverse inputs)

handleInputs :: Int-> [[Int]] -> IO()
handleInputs 0 _ = return()
handleInputs _ [] = return()
handleInputs n (x:xs) = do
    let profittuten = x!!0
    let profittmed = x!!1
    let kostnad = x!!2
    if profittmed-profittuten > kostnad then putStrLn "advertise"
    else if profittmed-profittuten < kostnad then putStrLn "do not advertise"
    else putStrLn "does not matter"
    handleInputs (n-1) xs


takeInput :: Int -> [[Int]] -> IO[[Int]]
takeInput n liste
    | n==0 = pure liste
    | otherwise = do
        input <- getLine
        let tallstrenger = words input
        let tall = [read x::Int | x<-tallstrenger]
        takeInput (n-1) (tall : liste)