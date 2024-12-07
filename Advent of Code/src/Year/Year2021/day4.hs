import Control.Monad ( liftM )
import Data.List (delete, find)
import Debug.Trace

main :: IO ()
main = do
    f <- liftM lines $ readFile "day4-input.txt"
    let draws = read $ "[" ++ head f ++ "]"
        players = readPlayers (drop 2 f)
    print (play draws players)
    print (playBad draws players)

type Player = [[Int]]

play :: [Int] -> [Player] -> Int
play [] _ = undefined
play (x:xs) ps = let nps = map (map (delete x)) ps
                 in  case find (elem []) nps of
                        Just  p -> x * sum (concat (take 5 p))
                        Nothing -> play xs nps

playBad :: [Int] -> [Player] -> Int
playBad [] _ = undefined
playBad (x:xs) ps = let nps = map (map (delete x)) ps
                    in  case filter (notElem []) nps of
                            []   -> x * (sum (concat (take 5 (head nps))))
                            nps' -> playBad xs nps'

readPlayers :: [String] -> [Player]
readPlayers [] = []
readPlayers xs = let rows = map (map read . words) $ take 5 xs
                     cols = map (\i -> map (!! i) rows) [0..4]
                 in  (rows ++ cols) : readPlayers (drop 6 xs)


