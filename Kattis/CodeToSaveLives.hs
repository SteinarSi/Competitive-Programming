import           Control.Monad (replicateM_)

main :: IO ()
main = do
    t <- fmap read getLine
    replicateM_ t $ do
        a <- fmap (read . filter (/= ' ')) getLine
        b <- fmap (read . filter (/= ' ')) getLine
        putStrLn $ unwords $ map pure $ show (a + b)
