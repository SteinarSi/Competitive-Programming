main :: IO ()
main = do
    s <- readFile "day1-input.txt"
    let xs = map read $ lines s
    print $ countIncreases xs
    print $ countSlidingIncreases xs

countIncreases :: [Int] -> Int
countIncreases xs = length [ () | (x, y) <- zip xs (tail xs), x < y]

countSlidingIncreases :: [Int] -> Int
countSlidingIncreases xs = countIncreases $ zipWith3 (\x y z -> x+y+z) xs (tail xs) (tail (tail xs))

test :: [Int]
test = [176,184,188,142,151,156,157,167,166,178,182,191,190,191,192,196,197,201,204,207,212,213,231,232,234,232,239,268,279]