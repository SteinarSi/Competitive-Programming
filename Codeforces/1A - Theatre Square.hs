import Data.Functor ((<&>))
import Control.Arrow ((>>>))

main :: IO ()
main = do
    [n,m,a] <- getLine <&> (words >>> map read)
    print (((n+a-1) `div` a) * ((m+a-1) `div` a))
