import           Control.Arrow ((>>>))
import           Control.Monad (replicateM, when)
import           Data.Char     (toLower)
import           Data.Functor  ((<&>))
import           Data.List     (sortOn, transpose)

main :: IO ()
main = do
    n:_ <- getLine <&> (words >>> map read)
    when (n /= 0) $ do
        replicateM n getLine >>= (
                transpose
            >>> sortOn (map toLower)
            >>> transpose
            >>> mapM_ putStrLn
            )
        putStrLn ""
        main
