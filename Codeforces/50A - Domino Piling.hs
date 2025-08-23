import Data.Functor ((<&>))
import Control.Arrow ((>>>))

main :: IO ()
main = do
    [n,m] <- getLine <&> (words >>> map read)
    print ((n*m) `div` 2)
