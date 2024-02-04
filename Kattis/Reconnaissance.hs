import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as B
import           Data.Maybe            (fromJust)

main :: IO ()
main = B.getContents >>= (
            B.lines
        >>> tail
        >>> map (readNums >>> (\(a:b:_) -> (a, b)))
        >>> binarySearch
        >>> print
    )

binarySearch :: [(Double, Double)] -> Double
binarySearch xs = bs $ upperBound (0,1)
    where
        upperBound :: (Double, Double) -> (Double, Double)
        upperBound (lo, hi) | distWhen lo < distWhen hi || distWhen hi < distWhen (hi+0.001) = (lo, hi)
                            | otherwise = upperBound (hi, hi*2)

        bs :: (Double, Double) -> Double
        bs (lo, hi)
            | abs (lo-hi) <= 0.00001 = distWhen mid
            | otherwise = case compare (distWhen mid) (distWhen (mid+0.001)) of
                        LT -> bs (lo, mid)
                        GT -> bs (mid, hi)
                        EQ -> distWhen mid
            where mid = (lo + hi) / 2

        distWhen :: Double -> Double
        distWhen t = let at = map (\(p,d) -> p + d*t) xs
                     in  maximum at - minimum at

readNums :: Num a => B.ByteString -> [a]
readNums = B.words >>> map (B.readInt >>> fromJust >>> fst >>> fromIntegral)
