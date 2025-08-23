main :: IO ()
main = do
    inn <- getLine

    let yss = map (\i -> concat [ f c i | (f,c) <- zip (cycle [peter, peter, wendy]) inn ]) [1..5]
        is = overlaps 0 (yss!!2)

    mapM_ (putStrLn . skipAt is 0) yss

peter :: Char -> Int -> String
peter _ 1 = "..#.."
peter _ 2 = ".#.#."
peter c 3 = "#." ++ [c] ++ ".#"
peter _ 4 = ".#.#."
peter _ 5 = "..#.."

wendy :: Char -> Int -> String
wendy _ 1 = "..*.."
wendy _ 2 = ".*.*."
wendy c 3 = "*." ++ [c] ++ ".*"
wendy _ 4 = ".*.*."
wendy _ 5 = "..*.."

overlaps :: Int -> String -> [Int]
overlaps _ []           = []
overlaps i ('#':'#':xs) = i : overlaps (i+2) xs
overlaps i ('#':'*':xs) = i : overlaps (i+2) xs
overlaps i ('*':'#':xs) = i+1 : overlaps (i+2) xs
overlaps i (x:xs)       = overlaps (i+1) xs

skipAt :: [Int] -> Int -> [a] -> [a]
skipAt [] _ xs = xs
skipAt _ _ [] = []
skipAt (i:is) j (x:xs) | i == j    = skipAt is (j+1) xs
                       | otherwise = x : skipAt (i:is) (j+1) xs
