import           Control.Arrow         ((>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntMap.Strict    as M
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:d:xs <- C.getContents <&> (C.words >>> map readInt)

    map ((`div` d) >>> (,1)) xs
        & M.fromListWith (+)
        & M.elems
        & map (\x -> x * (x-1) `div` 2)
        & sum
        & print

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
