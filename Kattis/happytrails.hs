import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readNum >>> (\[a,d] -> d * sin (a*pi / 180)))
        >>> sum
        >>> printf "%.2f\n"
    )

readNum :: C.ByteString -> Double
readNum = C.readInt >>> fromJust >>> fst >>> fromIntegral
