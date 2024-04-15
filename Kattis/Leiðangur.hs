main :: IO ()
main = getLine >>= putStrLn . solve []

solve :: String -> String -> String
solve b []             = count 0 0 0 b
solve b ('.':xs)       = solve b xs
solve [] (x:xs) | x `elem` "PGO" = "Neibb"
                | otherwise = solve [x] xs
solve (b:bs) (x:xs) = case (b,x) of
    ('p', 'P') -> solve bs xs
    (_, 'P')   -> solve bs (x:xs)
    ('g', 'G') -> solve bs xs
    (_, 'G')   -> solve bs (x:xs)
    ('o', 'O') -> solve bs xs
    (_, 'O')   -> solve bs (x:xs)
    (_, _)     -> solve (x:b:bs) xs

count :: Int -> Int -> Int -> String -> String
count p g o []       = init . unlines $ map show [p,g,o]
count p g o ('p':xs) = count (p+1) g o xs
count p g o ('g':xs) = count p (g+1) o xs
count p g o ('o':xs) = count p g (o+1) xs
