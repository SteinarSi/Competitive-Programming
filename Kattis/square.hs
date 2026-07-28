import           Control.Monad (replicateM_)
import           Data.List     (intersperse)

main :: IO ()
main = do
    d <- fmap read getLine

    if d == 1
        then putStrLn "*"
        else do
            putStrLn (intersperse ' ' (replicate d '*'))
            replicateM_ (d-2) (putStrLn ('*' : (replicate (2*d-3) ' ') <> "*"))
            putStrLn (intersperse ' ' (replicate d '*'))
