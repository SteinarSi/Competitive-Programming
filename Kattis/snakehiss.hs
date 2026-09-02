main :: IO ()
main = do
    n <- getLine
    putStrLn ("His" <> replicate (read n) 's' <> "!")
