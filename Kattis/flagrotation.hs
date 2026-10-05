import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntMap.Strict    as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:xs <- C.getContents <&> (C.words >>> map readInt)

    map (,1) xs
        & M.fromListWith (+)
        & M.elems
        & map (^2)
        & sum
        & (n^2 -)
        & print

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
