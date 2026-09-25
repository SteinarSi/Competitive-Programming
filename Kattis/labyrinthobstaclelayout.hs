main :: IO ()
main = do
    n <- read <$> getLine
    putStr $ unlines (replicate n '.' : replicate (n-2) ('.' : replicate (n-2) '#' <> ".") <> [replicate n '.'])
