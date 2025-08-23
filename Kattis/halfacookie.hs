import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> map (C.words >>> map readDouble >>> solve)
        >>> C.unlines
        >>> C.putStr
    )

solve :: [Double] -> C.ByteString
solve [r,x,y] | d > r     = C.pack "miss"
              | otherwise = C.pack (printf "%.4f %.4f" full part)
    where
        d = sqrt (x^2 + y^2)
        θ = 2 * acos (d / r)
        part = r^2 * (θ - sin θ)/2
        full = r^2 * pi - part

readDouble :: C.ByteString -> Double
readDouble s | C.head s == '-'  = negate (readDouble (C.tail s))
             | C.length r1 <= 1 = fromIntegral int
             | otherwise        = fromIntegral int + float
    where (int, r1) | C.head s == '.' = (0, s)
                    | otherwise       = fromJust (C.readInt s)
          Just (dec, r2) = C.readInt (C.tail r1)
          float | C.length r1 <= 1 = 0
                | otherwise = fromIntegral dec / (10^^fromIntegral (C.length r1 - C.length r2 - 1))
