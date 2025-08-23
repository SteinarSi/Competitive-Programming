main :: IO()
main = getLine >>= putStrLn . \(x:xs) -> foldl (\x y-> if last x /= y then x ++ [y] else x) [x] xs