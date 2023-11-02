main :: IO ()
main = getLine >> getLine >>= putStrLn . unlines . words
