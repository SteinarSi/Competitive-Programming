import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntMap.Strict    as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:k:xs <- C.getContents <&> (C.words >>> map readInt)
    xs
        & map (,1)
        & M.fromListWith (+)
        & M.elems
        & map (min k)
        & sum
        & print

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
