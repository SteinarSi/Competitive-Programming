import           Control.Monad (forM_)
import           Data.Bool     (bool)

main :: IO ()
main = do
    n:p:s:_ <- fmap words getLine
    queries <- fmap (map (tail . words) . lines) getContents
    forM_ queries (putStrLn . bool "REMOVE" "KEEP" . elem p)
