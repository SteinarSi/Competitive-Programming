import           Control.Monad (replicateM_)

main :: IO ()
main = do
    cases <- fmap read getLine
    replicateM_ cases $ do
        [n, m] <- fmap (map read . words) getLine
        replicateM_ m getLine
        print (n-1)
