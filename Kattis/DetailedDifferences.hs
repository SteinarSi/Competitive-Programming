import           Control.Monad (join, replicateM_)
import           Data.Bool     (bool)
import           Data.Function ((&))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    t <- getLine <&> read
    replicateM_ t $ do
        xs <- getLine
        ys <- getLine
        [xs, ys, diff xs ys]
            & unlines
            & putStrLn

diff :: String -> String -> String
diff = zipWith ((bool '*' '.' .) . (==))
