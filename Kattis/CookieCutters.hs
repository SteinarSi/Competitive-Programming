import           Control.Arrow            ((>>>), (&&&), (***))
import qualified Data.ByteString.Char8 as C
import           Data.Function            ((&))
import           Data.Maybe               (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> tail
        >>> map (C.words >>> map readDouble)
        >>> (init >>> map (head &&& last)) &&& (last >>> head)
        >>> uncurry resize
        >>> shift
        >>> map (\(a,b) -> C.unwords $ map (show >>> C.pack) [a,b])
        >>> C.unlines
        >>> C.putStr
    )

resize :: [(Double,Double)] -> Double -> [(Double,Double)]
resize points target = map (scale *** scale) points
    where scale = (sqrt (target / shoelace points) *)

shift :: [(Double,Double)] -> [(Double,Double)]
shift points = map (shiftX *** shiftY) points
    where
        shiftX = subtract (minimum (map fst points))
        shiftY = subtract (minimum (map snd points))

shoelace :: [(Double, Double)] -> Double
shoelace xs = abs (sum (zipWith (\(x1,y1) (x2,y2) -> (y1+y2) * (x1-x2)) xs (tail xs ++ [head xs]))) / 2

readDouble :: C.ByteString -> Double
readDouble s | C.head s == '-' = negate (readDouble (C.tail s))
             | C.length r1 <= 1 = fromIntegral int
             | otherwise = fromIntegral int + float
    where (int, r1) | C.head s == '.' = (0, s)
                    | otherwise = fromJust (C.readInt s)
          Just (dec, r2) = C.readInt (C.tail r1)
          float | C.length r1 <= 1 = 0
                | otherwise = fromIntegral dec / (10^^fromIntegral (C.length r1 - C.length r2 - 1))
