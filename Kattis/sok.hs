import           Control.Arrow ((>>>))
import           Data.Function ((&))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    amounts <- getLine <&> (words >>> map read)
    ratios  <- getLine <&> (words >>> map read)

    let canMake = minimum $ zipWith (/) amounts ratios

    zipWith (\a r -> a - r * canMake) amounts ratios
        & map show
        & unwords
        & putStrLn
