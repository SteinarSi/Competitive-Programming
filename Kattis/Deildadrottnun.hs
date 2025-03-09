import           Control.Arrow (second, (>>>))
import           Data.Function ((&))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    n <- getLine <&> read

    let m = last $ takeWhile (gauss >>> (<=n)) [1..]

    print m
    splitAt (m - n + gauss m) [1..m]
        & second (map succ)
        & uncurry (<>)
        & map show
        & unwords
        & putStrLn

gauss :: Int -> Int
gauss n = (n*(n+1)) `div` 2
