import Control.Monad (replicateM_)

main = do
    n <- fmap read getLine
    replicateM_ n (getLine >>= \s -> case words s of
            ("simon":"says":xs) -> putStrLn (unwords xs)
            _ -> putChar '\n'
        )