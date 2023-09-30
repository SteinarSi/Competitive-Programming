main :: IO ()
main = do
    getLine
    friends <- fmap words getLine
    putStrLn (friends !! (12 `mod` length friends))