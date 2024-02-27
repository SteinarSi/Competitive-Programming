import           Control.Arrow ((>>>))
import           Control.Monad (forM_, replicateM)
import           Data.Function ((&))
import           Data.Functor  ((<&>))
import           Data.List     (transpose)

main :: IO ()
main = do
    n <- fmap read getLine
    forM_ [1..n] $ \i -> do
        putStrLn ("Test " <> show i)
        [m, n] <- getLine <&> (words >>> map read)
        replicateM m getLine >>= (
                map reverse
            >>> transpose
            >>> map reverse
            >>> transpose
            >>> mapM_ putStrLn
            )
