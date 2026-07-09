import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [n,m] <- getLine <&> (words >>> map read)
    putStrLn $ if n `mod` m == 0
        then "NO"
        else "YES\n" <> unwords (take n (cycle ("1" : replicate (m-1) "2")))
