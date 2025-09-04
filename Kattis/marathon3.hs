import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readDouble >>> head &&& last)
        >>> solve
        >>> print
    )

solve :: [(Double,Double)] -> Double
solve xs = xs
    & (map fst >>> minimum >>> pred) &&& (map snd >>> maximum >>> succ)
    & bin 40
  where
    bin :: Int -> (Double,Double) -> Double
    bin i (lo,hi)
        | i <= 0 = mi
        | chances mi < 0.5 = bin (i-1) (lo,mi)
        | otherwise = bin (i-1) (mi,hi)
      where
        mi = (lo+hi) / 2

    chances :: Double -> Double
    chances s = product (map chance xs)
      where
        chance :: (Double,Double) -> Double
        chance (l,r)
            | s <= l = 1
            | s >= r = 0
            | otherwise = (r-s) / (r-l)

readDouble :: C.ByteString -> Double
readDouble s | C.head s == '-' = negate (readDouble (C.tail s))
             | C.length r1 <= 1 = fromIntegral int
             | otherwise = fromIntegral int + float
    where (int, r1) | C.head s == '.' = (0, s)
                    | otherwise = fromJust (C.readInt s)
          Just (dec, r2) = C.readInt (C.tail r1)
          float | C.length r1 <= 1 = 0
                | otherwise = fromIntegral dec / (10^^fromIntegral (C.length r1 - C.length r2 - 1))
