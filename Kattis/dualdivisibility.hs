import           Control.Arrow ((>>>))
import           Data.Function ((&))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [a,b] <- getLine <&> (words >>> map read)

    [b,2*b..]
        & takeWhile (<=a)
        & filter (mod a >>> (==0))
        & length
        & print
