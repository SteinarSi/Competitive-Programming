import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import qualified Data.IntMap.Strict    as M
import           Data.Maybe            (fromJust)
import           Data.Tuple            (swap)
import           Text.Printf           (printf)

main :: IO ()
main = C.getContents >>= (
            C.lines
        >>> drop 1
        >>> map (C.words >>> map readInt >>> \[c,r] -> (c,(r,1)))
        >>> M.fromListWith (\(r1,s1) (r2,s2) -> (r1+r2,s1+s2))
        >>> M.assocs
        >>> map (\(i,(r,s)) -> (r `div` s,i))
        >>> minimum
        >>> swap
        >>> uncurry (printf "%d\n%d\n")
    )

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
