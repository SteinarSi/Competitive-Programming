import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    (r,_):points <- C.getContents <&> (C.lines >>> map (C.words >>> map readDouble >>> head &&& last))

    let vectors = (1,0) : zipWith (\(x1,y1) (x2,y2) -> (x2-x1,y2-y1)) (cycle points) (tail (cycle points))
    print (goat r vectors)

goat :: Double -> [(Double,Double)] -> Double
goat r (a@(x1,y1):b@(x2,y2):xs)
    | magnitude b >= r = dist
    | otherwise        = dist + goat (r - magnitude b) (b:xs)
  where
    dist  = r * acos ((x1*x2 + y1*y2) / (magnitude a * magnitude b))

magnitude :: (Double, Double) -> Double
magnitude (x,y) = sqrt (x^2 + y^2)

readDouble :: C.ByteString -> Double
readDouble s | C.head s == '-' = negate (readDouble (C.tail s))
             | C.length r1 <= 1 = fromIntegral int
             | otherwise = fromIntegral int + float
    where (int, r1) | C.head s == '.' = (0, s)
                    | otherwise = fromJust (C.readInt s)
          Just (dec, r2) = C.readInt (C.tail r1)
          float | C.length r1 <= 1 = 0
                | otherwise = fromIntegral dec / (10^^fromIntegral (C.length r1 - C.length r2 - 1))
