import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Char             (digitToInt)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.unpack >>> words >>> (\[k,xs] -> map (digitToInt >>> (+ read k) >>> show >>> last) xs))
        >>> unlines
        >>> putStr
    )
