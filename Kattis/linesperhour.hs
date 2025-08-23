import           Data.Function ((&))
import           Data.List     (sort)

main :: IO ()
main = do
    n:lph:xs <- fmap (map read . words) getContents
    sort xs
        & scanl (+) 0
        & takeWhile (<=5*lph)
        & length
        & pred
        & print
