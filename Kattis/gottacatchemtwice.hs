import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Maybe            (fromJust)
import qualified Data.Set              as S

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> S.fromList
        >>> S.size
        >>> succ
        >>> print
    )

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
