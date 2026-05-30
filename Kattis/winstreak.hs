import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.List             (group)
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt >>> \[s,t] -> s > t)
        >>> group
        >>> filter head
        >>> map length
        >>> (0:)
        >>> maximum
        >>> print
    )

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
