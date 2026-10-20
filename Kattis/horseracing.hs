import           Control.Arrow (first, (>>>))
import           Data.Function ((&))
import           Data.Functor  ((<&>))

main :: IO ()
main = do
    [_,d]:xs <- getContents <&> (lines >>> map (words >>> map read))

    let
        time :: [Double] -> Double
        time [p,t]
            | t >= d    = d / p
            | otherwise = t / p + (d-t) / (p/2)

    zip xs [0..]
        & map (first time)
        & minimum
        & snd
        & print
