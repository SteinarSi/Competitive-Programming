import           Control.Arrow ((>>>))
import           Control.Monad (replicateM, when)
import           Data.Char     (toLower)
import           Data.List     (sort)
import           Data.Map      (Map, assocs, empty, insertWith)

main :: IO ()
main = loop 1

loop :: Int -> IO ()
loop i = do
    n <- fmap read getLine
    when (n /= 0) $ do
        putStrLn ("List " ++ show i ++ ":")
        replicateM n getLine >>= (
                    map (words >>> last >>> map toLower)
                >>> foldl (\m x -> insertWith (+) x 1 m) empty
                >>> assocs
                >>> sort
                >>> mapM_ (\(n,c) -> putStrLn (n ++ " | " ++ show c))
            )
        loop (i+1)
