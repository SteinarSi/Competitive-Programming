import           Control.Arrow         ((>>>))
import           Data.Array.Unboxed    (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    _:xs:[m]:ys:rest <- C.getContents <&> (C.lines >>> map (C.words >>> map readInt))

    let top :: UArray Int Int
        top = listArray (1,m) ys

    init rest
        & zipWith (\x (_:y) -> x + minimum (map (top!) y)) xs
        & minimum
        & div (last (last rest))
        & pred
        & max 0
        & print

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
