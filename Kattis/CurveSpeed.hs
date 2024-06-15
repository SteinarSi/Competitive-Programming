import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> mapM_ (C.words
            >>> map readDouble
            >>> (\(a:b:_) -> solve a b)
            >>> C.putStrLn
        )
    )

solve :: Double -> Double -> C.ByteString
solve r s = sqrt (r * (s + 0.16) / 0.067)
        & round
        & show
        & C.pack

readDouble :: C.ByteString -> Double
readDouble s | C.length r1 <= 1 = fromIntegral int
             | otherwise = fromIntegral int + float
    where (int, r1) | C.head s == '.' = (0, s)
                    | otherwise = fromJust (C.readInt s)
          Just (dec, r2) = C.readInt (C.tail r1)
          float | C.length r1 <= 1 = 0
                | otherwise = fromIntegral dec / (10^^fromIntegral (C.length r1 - C.length r2 - 1))
