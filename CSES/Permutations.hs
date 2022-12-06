main :: IO ()
main = getLine >>= putStrLn . perms . read

perms :: Int -> String
perms 1 = "1"
perms 2 = "NO SOLUTION"
perms 3 = "NO SOLUTION"
perms n = unwords $ map show $ [2,4..n] ++ [1,3..n]
