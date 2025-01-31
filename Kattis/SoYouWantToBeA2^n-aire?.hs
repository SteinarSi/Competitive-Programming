import           Control.Arrow         ((&&&), (>>>))
import           Data.Bits             (shiftL)
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> init
        >>> map (C.words
            >>> (last >>> readDouble)
                &&&
                (head >>> readInt)
            >>> uncurry solve)
        >>> C.unlines
        >>> C.putStr
    )

solve :: Double -> Int -> C.ByteString
solve t = twonaire 1 >>> printf "%.3f" >>> C.pack
    where
        twonaire :: Int -> Int -> Double
        twonaire m 0 = fromIntegral m
        twonaire m n = pWorthIt * (((sweetSpot + 1) / 2) * r) + pNotWorthIt * fromIntegral m
            where
                r = twonaire (shiftL m 1) (n-1)
                sweetSpot = max t (fromIntegral m / r)
                pWorthIt = (1-sweetSpot) / (1 - t)
                pNotWorthIt = 1 - pWorthIt

readDouble :: C.ByteString -> Double
readDouble s | C.head s == '-' = negate (readDouble (C.tail s))
             | C.length r1 <= 1 = fromIntegral int
             | otherwise = fromIntegral int + float
    where (int, r1) | C.head s == '.' = (0, s)
                    | otherwise = fromJust (C.readInt s)
          Just (dec, r2) = C.readInt (C.tail r1)
          float | C.length r1 <= 1 = 0
                | otherwise = fromIntegral dec / (10^^fromIntegral (C.length r1 - C.length r2 - 1))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
