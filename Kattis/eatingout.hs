import           Control.Arrow ((>>>))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    (m:abc) <- getContents <&> (words >>> map read)
    putStrLn $ if 2*m >= sum abc
        then "possible"
        else "impossible"
