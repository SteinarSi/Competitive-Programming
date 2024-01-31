import           Control.Monad (replicateM_)
import           Data.Char     (toLower)

main :: IO ()
main = do
    n <- fmap read getLine
    replicateM_ n $ do
        phrase <- fmap (map toLower) getLine
        putStrLn $ case filter (`notElem` phrase) ['a'..'z'] of
            [] -> "pangram"
            xs -> "missing " ++ xs
