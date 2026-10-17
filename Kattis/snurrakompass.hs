import           Control.Arrow ((>>>))
import           Data.Function ((&))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [a,b] <- getContents <&> (words >>> map read)
    iterate ((+ gcd a b) >>> (`mod` 360)) 0
        & drop 1
        & takeWhile (/=0)
        & (0:)
        & length
        & print
