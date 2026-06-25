import           Control.Arrow         ((&&&), (***), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         (on)
import qualified Data.IntMap.Strict    as IM
import           Data.List             (maximumBy)
import qualified Data.Map.Strict       as M
import           Data.Maybe            (fromJust)
import           Text.Printf           (printf)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> head &&& (last >>> readInt >>> (`IM.singleton` 1)))
        >>> M.fromListWith (IM.unionWith (+))
        >>> M.toAscList
        >>> map (C.unpack *** (IM.toDescList >>> maximumBy (compare `on` snd) >>> fst) >>> uncurry (printf "%s %d"))
        >>> unlines
        >>> putStr
    )

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
