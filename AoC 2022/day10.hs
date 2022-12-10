import Data.Char (toUpper)
import Data.List.Split (chunksOf)

data Instruction = NOOP | ADDX Int deriving (Read, Show)

main :: IO ()
main = do
    inn <- readFile "day10-input.txt"
    let (n, s) = cycling 1 1 0 0 "" $ map (read . map toUpper) $ lines inn
    print n
    mapM_ putStrLn $ chunksOf 40 s

cycling :: Int -> Int -> Int -> Int -> String -> [Instruction] -> (Int, String)
cycling _ _ _ n s [] = (n,reverse s)
cycling y cycle toAdd n s (NOOP:xs)   = cycling (y+toAdd) (cycle+1) 0 (value y cycle + n) (pixel (y+toAdd) cycle : s) xs
cycling y cycle toAdd n s (ADDX x:xs) = cycling (y+toAdd) (cycle+1) x (value y cycle + n) (pixel (y+toAdd) cycle : s) (NOOP:xs)

pixel :: Int -> Int -> Char
pixel x cycle | abs (x - cycle `mod` 40) <= 1 = '#'
              | otherwise = '.'

value :: Int -> Int -> Int
value x cycle | cycle `elem` [20, 60, 100, 140, 180, 220] = x * cycle
              | otherwise = 0
