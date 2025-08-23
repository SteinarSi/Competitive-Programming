import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap           as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = C.getContents >>= (
            C.words
        >>> tail
        >>> map (readInt >>> succ >>> (,1))
        >>> M.fromListWith (+)
        >>> M.assocs
        >>> map (\(key,val) -> key * ((val+key-1) `div` key))
        >>> sum
        >>> print
    )

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
