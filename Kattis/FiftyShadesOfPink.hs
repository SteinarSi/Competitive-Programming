import Data.Char
import Data.List

main :: IO ()
main = do
    antall <- getLine
    inputs <- getInputs (read antall::Int) []
    let lowered = [[toLower c | c<-s] | s<-inputs]
    let total = countPink lowered
    if total == 0 then putStrLn "I must watch Star Wars with my daughter"
    else putStrLn (show (countPink lowered))
    
countPink :: [String] -> Int
countPink [] = 0
countPink (x:xs)
    | isInfixOf "pink" x || isInfixOf "rose" x = 1 + countPink xs
    | otherwise = countPink xs

getInputs :: Int -> [String] -> IO [String]
getInputs antall liste = do
    case antall of
        0 -> pure liste
        _ -> do
            input <- getLine
            getInputs (antall-1) (input : liste)