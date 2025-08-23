import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Complex          (Complex (..), magnitude)
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> map (C.words >>> \[x,y,r] -> mandelbrot (readDouble x :+ readDouble y) 0 (readInt r))
        >>> zipWith (printf "Case %d: %s") [1::Int ..]
        >>> unlines
        >>> putStr
    )

mandelbrot :: Complex Double -> Complex Double -> Int -> String
mandelbrot xy z _ | magnitude z > 2 = "OUT"
mandelbrot xy z 0 = "IN"
mandelbrot xy z r = mandelbrot xy (z^2 + xy) (r-1)

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst

readDouble :: C.ByteString -> Double
readDouble s | C.head s == '-' = negate (readDouble (C.tail s))
             | C.length r1 <= 1 = fromIntegral int
             | otherwise = fromIntegral int + float
    where (int, r1) | C.head s == '.' = (0, s)
                    | otherwise = fromJust (C.readInt s)
          Just (dec, r2) = C.readInt (C.tail r1)
          float | C.length r1 <= 1 = 0
                | otherwise = fromIntegral dec / (10^^fromIntegral (C.length r1 - C.length r2 - 1))
