import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (toUpper)
import           Data.Maybe            (fromJust)

data Event =
      Buy Int Double
    | Sell Int Double
    | Split Int
    | Merge Int
    | Die Double
  deriving (Read,Show)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> map (C.unpack >>> (\(x:xs) -> read (toUpper x : xs)))
        >>> solve (0,0)
        >>> print
    )

solve :: (Int,Double) -> [Event] -> Double
solve (s,p) (e:es) = case e of
    (Die y)    -> fromIntegral s * (y - max 0 (0.3*(y-p)))
    (Buy x y)  -> solve (s+x,(fromIntegral s * p + fromIntegral x * y) / fromIntegral (s+x)) es
    (Sell x y) -> solve (s-x,p) es
    (Split x)  -> solve (s*x,p/fromIntegral x) es
    (Merge x)  -> solve (s `div` x, fromIntegral x * p) es

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
