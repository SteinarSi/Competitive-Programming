import           Control.Arrow ((>>>))
import           Data.Function ((&))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [s,p,a,b] <- getContents <&> (words >>> map read)

    [0..1+p-2*s]
        & map (\y -> b*y + a * (1 - (s+y) + (p+y) `div` 2))
        & minimum
        & print
