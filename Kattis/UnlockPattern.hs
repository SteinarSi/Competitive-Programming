import           Control.Arrow ((>>>))
import           Control.Monad (ap)
import           Data.List     (sort)

main :: IO ()
main = getContents >>= (
            words
        >>> flip zip [(r,c) | r <- [1..3], c <- [1..3]]
        >>> sort
        >>> map snd
        >>> ap (zipWith dist) tail
        >>> sum
        >>> print
    )

dist :: (Double,Double) -> (Double,Double) -> Double
dist (x1,y1) (x2,y2) = sqrt (abs (x1-x2)**2 + abs (y1-y2)**2)
