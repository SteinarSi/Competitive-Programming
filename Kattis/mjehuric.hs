import           Control.Arrow ((>>>))

main :: IO ()
main = getLine >>= (
            words
        >>> map read
        >>> solve 0
        >>> map (map show >>> unwords)
        >>> unlines
        >>> putStr
    )

solve :: Int -> [Int] -> [[Int]]
solve i xs | xs == next && i == 0 = []
           | xs == next = solve 0 xs
           | otherwise  = next : solve (i+1) next
    where 
        (a, b) = splitAt i xs
        next   = a ++ bubble b

        bubble :: [Int] -> [Int]
        bubble [] = []
        bubble [x] = [x]
        bubble (x:y:xs) | x > y     = y : x : xs
                        | otherwise = x : bubble (y:xs)
