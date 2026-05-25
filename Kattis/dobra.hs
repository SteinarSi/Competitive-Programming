main :: IO ()
main = getLine >>= (print . solve False 0 0)

solve :: Bool -> Int -> Int -> String -> Int
solve _ 3 _ _ = 0
solve _ _ 3 _ = 0
solve False _ _ "" = 0
solve True  _ _ "" = 1
solve b c v ('_':xs) = 5 * solve b 0 (v+1) xs + 20 * solve b (c+1) 0 xs + solve True (c+1) 0 xs
solve b c v (x:xs)
    | x `elem` "AEIOU" = solve b 0 (v+1) xs
    | otherwise        = solve (b || x == 'L') (c+1) 0 xs
