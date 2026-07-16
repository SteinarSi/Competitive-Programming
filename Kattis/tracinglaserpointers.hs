import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (sort)
import           Data.Maybe            (fromJust, mapMaybe)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> mapMaybe (C.words >>> \[x,y,m,n] -> (,n) <$> intersect (readDouble x) (readDouble y) (readDouble m))
        >>> sort
        >>> map snd
        >>> C.unlines
        >>> C.putStr
    )

intersect :: Double -> Double -> Double -> Maybe Double
intersect x y m
    | y > 0 && m >= 0 = Nothing
    | y < 0 && m <= 0 = Nothing
    | otherwise       = Just (x - y / m)

readDouble :: C.ByteString -> Double
readDouble s | C.head s == '-' = negate (readDouble (C.tail s))
             | C.length r1 <= 1 = fromIntegral int
             | otherwise = fromIntegral int + float
    where (int, r1) | C.head s == '.' = (0, s)
                    | otherwise = fromJust (C.readInt s)
          Just (dec, r2) = C.readInt (C.tail r1)
          float | C.length r1 <= 1 = 0
                | otherwise = fromIntegral dec / (10^^fromIntegral (C.length r1 - C.length r2 - 1))
