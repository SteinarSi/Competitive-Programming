main :: IO ()
main = getLine >>= print . solve 0

solve :: Int -> String -> Int
solve r "" = 0
solve r ('<':xs) = r + solve r xs
solve r ('>':xs) = solve (r+1) xs
solve r (_:xs) = solve r xs
