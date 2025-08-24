import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readNum >>> \[a,r1,r2] -> sector a r2 - sector a r1)
        >>> sum
        >>> print
    )

sector :: Double -> Double -> Double
sector a r = pi * r^2 * a / 360

readNum :: Num a => C.ByteString -> a
readNum = C.readInt >>> fromJust >>> fst >>> fromIntegral
