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
        >>> M.assocs
        >>> map (\(x,c) -> x+1 - (x+1-c) `div` 2)
        >>> maximum
        >>> print
    )

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
