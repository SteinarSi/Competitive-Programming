import           Control.Arrow         ((>>>))
import           Data.Array.Unboxed    (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Function         ((&))
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:k:xs <- C.getContents <&> (C.words >>> map readInt)

    let psum = listArray (0,n) (scanl (+) 0 xs) :: UArray Int Int

    [k..n]
        & map (\j -> psum ! j - psum ! (j-k))
        & maximum
        & print

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
