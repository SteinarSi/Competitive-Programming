main :: IO ()
main = getLine >>= print . solve 0 1

solve :: Int -> Int -> String -> Int
solve n s []       = n
solve n s ('B':xs) = solve (min (n+1) (s+2)) (min (n+2) (s+1)) xs
solve n s ('N':xs) = solve (min (n+1) (s+1)) (min (n+1) s) xs
solve n s ('S':xs) = solve (min n (s+1)) (min (n+1) (s+1)) xs
solve _ _ _        = error "bruh"
