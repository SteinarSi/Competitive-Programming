import Data.List (nub)

main :: IO ()
main = readFile "day6-input.txt" >>= \inn -> print (marker 4 inn, marker 14 inn)

marker :: Int -> String -> Int
marker n xs | nub (take n xs) == take n xs = n
            | otherwise = 1 + marker n (tail xs)
