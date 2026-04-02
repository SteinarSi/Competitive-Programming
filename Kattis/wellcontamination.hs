import           Control.Arrow         ((>>>))
import           Data.Array.Unboxed    (UArray, assocs, inRange, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    r:c:xs <- C.getContents <&> C.words
    let rng = ((1,1),(readInt r, readInt c))
        grid = listArray rng (concatMap C.unpack xs) :: UArray (Int,Int) Char
    assocs grid
        & filter (\(_,c) -> c =='W')
        & filter (\((y,x),_) -> any (\v -> inRange rng v && grid ! v == 'O') [(y-1,x),(y+1,x),(y,x-1),(y,x+1)])
        & length
        & print

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
