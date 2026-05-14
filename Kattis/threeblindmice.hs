import           Control.Arrow ((>>>))
import           Data.Function ((&))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    d:w:ms <- getContents <&> (words >>> map read)
    ms
        & map (\m -> 2 * m * (d / (m+w)))
        & sum
        & round
        & print
