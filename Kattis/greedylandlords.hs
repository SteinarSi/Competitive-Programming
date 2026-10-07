import           Control.Arrow         ((>>>))
import           Data.Array.Unboxed    (UArray, listArray, (!))
import qualified Data.ByteString.Char8 as C
import           Data.Functor          ((<&>))
import           Data.Maybe            (fromJust)

main :: IO ()
main = do
    n:xs <- C.getContents <&> (C.words >>> map readInt)

    let
        sumLeft :: UArray Int Int
        sumLeft = listArray (0,n) (scanl (+) 0 xs)

        sumRight :: UArray Int Int
        sumRight = listArray (0,n) (scanl (+) 0 (reverse xs))

        steal :: UArray Int Int -> Int -> Int
        steal psum i = psum ! (i-1 + (n - i + 2) `div` 2) - psum ! (i-1)

    print (maximum (map (steal sumLeft) [1..n] <> map (steal sumRight) [1..n]))

readInt :: C.ByteString -> Int
readInt = C.readInt >>> fromJust >>> fst
