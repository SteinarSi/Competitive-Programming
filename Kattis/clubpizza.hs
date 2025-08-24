import           Control.Arrow         ((&&&), (>>>))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import qualified Data.IntMap.Strict    as M
import           Data.List             (sort)
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    (_,c):xs <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt >>> head &&& last))

    M.fromListWith min xs
        & M.elems
        & sort
        & scanl1 (+)
        & takeWhile (<=c)
        & length
        & print

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
