import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap.Strict    as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> drop 1
        >>> map (readInt >>> (,1))
        >>> M.fromListWith (+)
        >>> M.elems
        >>> map pred
        >>> sum
        >>> print
    )

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
