main :: IO ()
main = do
    n <- fmap read getLine
    putStr (unlines (map show [n,n-1..1]))
