import           Control.Arrow    ((>>>))
import           Control.Monad    (filterM, when)
import           Control.Monad.ST (ST, runST)
import           Data.Array.Base  (STUArray, newArray, readArray, writeArray)
import           Data.Function    ((&))
import           Data.Functor     ((<&>))

main :: IO ()
main = do
    n <- getLine <&> read

    print $ runST $ do
        sieve <- newArray (2,n-1) True :: ST s (STUArray s Int Bool)
        [2..n-1]
            & filterM (\p -> do
                s <- readArray sieve p
                when s (mapM_ (flip (writeArray sieve) False) [p+p,p+p+p .. n-1])
                pure s
            )
            <&> sum
