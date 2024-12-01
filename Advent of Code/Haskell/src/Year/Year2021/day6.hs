import Data.Map (Map, (!), fromList, adjust, elems)
import Data.List (foldl')

main :: IO ()
main = do
    s <- readFile "day6-input.txt"
    let fish = read ("[" ++ s ++ "]")
    print (sum (elems (loop (initial fish) 80)))
    print (sum (elems (loop (initial fish) 256)))

initial :: [Int] -> Map Int Int
initial = foldl' (\m f -> adjust (1+) f m) (fromList (map (\i -> (i, 0)) [0..8]))

loop :: Map Int Int -> Int -> Map Int Int
loop m 0 = m
loop m n = loop (fromList (map (\i -> (i-1, m!i)) [1..6] ++ [(6, m!7 + m!0), (7, m!8), (8, m!0)])) (n-1)