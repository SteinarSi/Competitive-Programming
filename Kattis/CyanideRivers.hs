import           Control.Arrow ((>>>))

main :: IO ()
main = getLine >>= (solve 0 0 >>> print)

solve :: Int -> Int -> String -> Int
solve best curr []       = (max best curr + 1) `div` 2
solve best curr ('1':xs) = solve (max best curr) 0 xs
solve best curr ('0':xs) = solve best (curr+1) xs
