main :: IO ()
main = do
    n <- read <$> getLine
    putStrLn $ if even n
        then "YES"
        else "NO"
