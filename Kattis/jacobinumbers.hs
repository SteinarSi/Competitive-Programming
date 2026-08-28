main :: IO ()
main = do
    n <- read <$> getLine
    putStrLn (show n <> "\n" <> unwords (map show (replicate n 1)))
