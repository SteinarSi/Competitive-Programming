import           Control.Monad (replicateM_)
import           Data.Function (on)
import           Data.List     (maximumBy)

main :: IO ()
main = do
    cases <- fmap read getLine
    replicateM_ cases $ do
        n <- fmap read getLine
        gears <- mapM (\x -> fmap ((\[a,b,c] -> (x, (a,b,c))) . map read . words) getLine) [1..n]
        print . fst $ maximumBy (compare `on` (torque . snd)) gears

torque :: (Double, Double, Double) -> Double
torque (a, b, c) = -a * r^2 + b * r + c
    where r = b / (2*a)
