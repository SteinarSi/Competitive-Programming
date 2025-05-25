import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [m,n,t] <- getLine <&> (words >>> map read)

    let possible = case t of
            1 -> n < 13 && m >= product [1..n]
            2 -> n < 31 && m >= 2^n
            3 -> m >= n^4
            4 -> m >= n^3
            5 -> m >= n^2
            6 -> m >= ceiling (fromInteger n * logBase 2 (fromInteger n))
            7 -> m >= n

    putStrLn $ if possible
        then "AC"
        else "TLE"
