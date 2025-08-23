import           Control.Arrow         ((>>>))
import           Data.Array.Unboxed    (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:xs <- C.getContents <&> (C.words >>> map readInt)

    let left  = listArray (1,n) (scanl max 0 xs)                     :: UArray Int Int
        right = listArray (1,n) (reverse (scanl max 0 (reverse xs))) :: UArray Int Int

    zipWith (\i x -> max 0 (min (left ! i) (right ! i) - x)) [1..] xs
        & maximum
        & print

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
