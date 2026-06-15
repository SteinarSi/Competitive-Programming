import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    (r,_):_:points <- C.getContents <&> (C.lines >>> map (C.words >>> map readDouble >>> head &&& last))

    let
        segments = lzip points
        vectors  = map (\((x1,y1),(x2,y2)) -> (x2-x1, y2-y1)) segments
        innerArea = shoelace segments
        rectangleAreas = sum (map (magnitude >>> (*r)) vectors)
        cornerAreas = sum (map (\(a@(x1,y1),b@(x2,y2)) -> r^2 * acos ((x1*x2 + y1*y2) / (magnitude a * magnitude b)) / 2) (lzip vectors))

    print ((innerArea + rectangleAreas + cornerAreas) / 1000000)

lzip :: [a] -> [(a,a)]
lzip xs = zip xs (last xs : init xs)

shoelace :: [((Double, Double),(Double, Double))] -> Double
shoelace = map (\((x1,y1),(x2,y2)) -> (y1+y2) * (x1-x2)) >>> sum >>> abs >>> (/2)

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
